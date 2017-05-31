namespace HeatherBotCore

open Tweetinvi
open Newtonsoft.Json
open System
open System.IO
open System.Text.RegularExpressions

type Config = {
    ConsumerKey: string
    ConsumerSecret: string
    AccessToken: string
    AccessTokenSecret: string
}

type Item = string option

type MarkovChain = Map<string,(Item*int) list>

type Generator = {
    FirstWords: (string*int) list
    MarkovChain: MarkovChain
}

type TweetItems = {
    FirstWords: string List
    Tuples: (Item*Item) List
}



module Twitter =
    let setConfig (filename:string) =
        let config =
            File.ReadAllText filename
            |> JsonConvert.DeserializeObject<Config>
        let credentials =
            Tweetinvi.Auth.CreateCredentials(
                config.ConsumerKey,
                config.ConsumerSecret,
                config.AccessToken,
                config.AccessTokenSecret)
        Tweetinvi.Auth.ApplicationCredentials <- credentials
        Tweetinvi.Auth.SetCredentials credentials
        credentials

    let getTweets (username:string) =
        let getPartialTweets (username:string) (sinceId: int64 option) =
            let parameters = 
                let temp = Tweetinvi.Parameters.UserTimelineParameters()
                temp.IncludeRTS <- false
                temp.MaximumNumberOfTweetsToRetrieve <- 3200
                temp.TrimUser <- true
                match sinceId with 
                | Some idNum -> temp.MaxId <- idNum
                | None -> ()
                temp
            Tweetinvi.TweetinviConfig.ApplicationSettings.HttpRequestTimeout <- 60000
            Tweetinvi.TweetinviConfig.CurrentThreadSettings.HttpRequestTimeout <- 60000 
            let tweets = Tweetinvi.Timeline.GetUserTimeline (username, parameters) |> List.ofSeq
            if not (List.isEmpty tweets) then
                System.Console.WriteLine ((List.length tweets).ToString())
                let minId = tweets |> List.map (fun (t:Models.ITweet) -> t.Id) |> List.min
                let nextMaxId = minId - 1L
                Some (tweets, Some nextMaxId)
            else None
        Seq.unfold (getPartialTweets username) None
        |> List.concat

module Process =
    let chooseRandom (choices:('a*int) list) =
        let dummy = choices |> List.head |> fst
        let total = choices |> List.sumBy snd
        let r = System.Random()
        let choice = r.Next(total)
        choices
        |> List.scan (fun (previousItem,runningTotal) (item,value) -> (item,value+runningTotal)) (dummy,0)
        |> List.tail
        |> List.skipWhile (fun (item,runningTotal) -> choice > runningTotal)
        |> List.head
        |> fst

    let removeSpecialCharacters (str:string) = 
        let sb = System.Text.StringBuilder()
        for c in str do
            if ((c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9') || c = '@' || c = ',' || c = '\'') then
                sb.Append(c) |> ignore
            else ()
        sb.ToString()

    let removeReplies (s:string) =
        let rgx = Regex("@([0-9]|[a-z]|[A-Z]|_)+")
        s.Split(' ')
        |> List.ofArray
        |> List.skipWhile (fun word -> rgx.IsMatch(word))
        |> (fun words ->
            if List.isEmpty words then "" else List.reduce (fun x y -> x + " " + y) words)
    let convertTweetToTweetItem (tweet: Models.ITweet) =
        let text = 
            tweet.Text
            |> removeReplies
        let sentences = 
            text.Split([|'.'|])
            |> List.ofArray
        let sentenceTuples =
            sentences
            |> List.choose(fun sentence ->
                    let words = 
                        sentence
                        |> (fun (s:string) -> s.Split())
                        |> Array.toList
                        |> List.map removeSpecialCharacters
                        |> List.filter (System.String.IsNullOrWhiteSpace >> not)
                    if List.isEmpty words then None else
                    let tuples =
                        if List.isEmpty words then List.Empty
                        else
                            words
                            |> List.map Some
                            |> List.pairwise
                            |> (fun x -> List.append x (List.singleton (Some (List.last words),None)))
                    let firstWord = tuples |> List.head |> fst |> Option.get
                    Some (firstWord,tuples))
        if List.isEmpty sentenceTuples then None
        else Some {FirstWords = sentenceTuples |> List.map fst; Tuples = sentenceTuples |> List.collect snd}

    let convertTweetItemsToMarkovChain (tweetItems: TweetItems list) =
        let mc = 
            tweetItems
            |> List.collect (fun ti ->  ti.Tuples)
            |> List.groupBy fst
            |> List.sortBy fst
            |> List.choose (fun (firstWord,listOfTuples) ->

                            match firstWord with 
                            | Some word ->
                                let secondWordMap = 
                                    listOfTuples
                                    |> List.countBy snd
                                Some (word, secondWordMap)
                            | None -> None)
            |> Map.ofList
        mc

    let convertTweetItemsToFirstWords (tweetItems: TweetItems list) =
        tweetItems
        |> List.collect (fun ti -> ti.FirstWords)
        |> List.countBy id
        

    let convertTweetsToGenerator (tweets: Models.ITweet list) =
        let tweetItems = 
            tweets
            |> List.choose convertTweetToTweetItem
        let markovChain = 
            convertTweetItemsToMarkovChain tweetItems
        let firstWords =
            convertTweetItemsToFirstWords tweetItems
        {FirstWords = firstWords; MarkovChain = markovChain}

    let runMarkovChain (g:Generator) =
        let firstWord = chooseRandom g.FirstWords
        List.unfold (fun w -> 
                        let wordsToChooseFrom = (Map.find w g.MarkovChain)
                        let nextWord = chooseRandom wordsToChooseFrom
                        match nextWord with
                        | Some w -> Some (w,w)
                        | None -> None) firstWord
        |> (fun x -> List.append (List.singleton firstWord) x)
        |> List.reduce (fun x y -> x + " " + y)

module HeatherTweetGenerator =
    type Amount =
        | Singular
        | Plural

    type Emotion = |StronglyNegative | Negative |Neutral | Positive | StronglyPositive
    type NounCategory =
        | Object of Amount
        | EObject of Amount
        | Person of Amount
        | Pet of Amount
        | Location
        | Event
    type Location =
        | Before
        | After

    type Adjective = {
        String: string;
        Emotion: (Emotion->Emotion);
        Weight: float;
        Location: Location;
        Categories: NounCategory seq}

    type Article =
        | NoArticle
        | Always of string
        | Sometimes of string
    type Noun = {
        String: string;
        Article: Article;
        Emotion: Emotion;
        Category: NounCategory}

    type Gerund = {
        String: string
        Emotion: (Emotion-> Emotion)
        Weight: float}
    type NounWithMeta = {
        Noun: Noun;
        Weight: float;
        Adjectives: Adjective seq;
        }

    type Sentence = 
        {Generator: unit -> string*Emotion;
         Weight: float
         }

    type SentenceAdjuster =
        {Adjuster: Sentence -> Sentence;
         Weight: float;
        }

    let sentenceAdjusterProbability = 0.5
    let random = new System.Random()
    let negateEmotion (e: Emotion) =
        match e with
        | StronglyNegative -> StronglyPositive
        | Negative -> Positive
        | Neutral -> Neutral
        | Positive -> Negative
        | StronglyPositive -> StronglyNegative
    let boostEmotion (e: Emotion) =
        match e with
        | StronglyNegative | Negative -> StronglyNegative
        | Neutral -> Neutral
        | StronglyPositive | Positive-> StronglyPositive
    let pick (s: (float*'t) seq) =
        let totalProbability =  s
                                |> Seq.map (fun x -> System.Math.Exp(fst x))
                                |> Seq.sum
        let pick = random.NextDouble()*totalProbability
        s
        |> Seq.scan (fun (runningTotal,last) current -> (runningTotal + System.Math.Exp(fst current), current)) 
            (0.0,s |> Seq.head)
        |> Seq.skipWhile (fun (value,current) -> value < pick)
        |> Seq.head
        |> snd
        |> snd
        
    let allSingularCategories = [Object Singular; EObject Singular; Person Singular; Pet Singular; Location; Event] |> List.toSeq
    let allPluralCategories = [Object Plural; EObject Plural; Person Plural; Pet Plural] |> List.toSeq
    let allCategories = Seq.append allSingularCategories allPluralCategories
    let allEmotions = [StronglyNegative; Negative; Neutral; Positive; StronglyPositive] |> List.toSeq
    let nouns = 
        [
        {Noun = {String= "Harry Potter"; Article = NoArticle; Emotion = Positive; Category = EObject Singular}; Weight = 1.0; Adjectives = Seq.empty<Adjective> }
        {Noun = {String= "Harry and the Potters"; Article = NoArticle; Emotion = Positive; Category = EObject Singular}; Weight = 1.0; Adjectives = Seq.empty<Adjective> }
        {Noun = {String= "tumblr"; Article = Sometimes "a"; Emotion = Positive; Category = EObject Singular}; Weight = 1.0; Adjectives = Seq.empty<Adjective> }
        {Noun = {String= "memes"; Article = NoArticle; Emotion = Neutral; Category = EObject Plural}; Weight = 1.0; Adjectives = [{String="dank";Emotion=id;Weight=1.0;Location=Before;Categories=Seq.empty<NounCategory>}] }
        {Noun = {String= "Gishwhes"; Article = NoArticle; Emotion = Positive; Category = EObject Singular}; Weight = 1.0; Adjectives = Seq.empty<Adjective> }
        {Noun = {String= "wizard rock"; Article = NoArticle; Emotion = Positive; Category = EObject Singular}; Weight = 1.0; Adjectives = Seq.empty<Adjective> }
        {Noun = {String= "Neuroscience"; Article = NoArticle; Emotion = Positive; Category = EObject Singular}; Weight = 1.0; Adjectives = Seq.empty<Adjective> }
        {Noun = {String= "twitter"; Article = NoArticle; Emotion = Neutral; Category = EObject Singular}; Weight = 0.3; Adjectives = Seq.empty<Adjective> }
        {Noun = {String= "twitter.com"; Article = NoArticle; Emotion = Neutral; Category = EObject Singular}; Weight = 0.3; Adjectives = Seq.empty<Adjective> }
        {Noun = {String= "twitter dot com"; Article = NoArticle; Emotion = Neutral; Category = EObject Singular}; Weight = 0.3; Adjectives = Seq.empty<Adjective> }
        {Noun = {String= "kpop"; Article = NoArticle; Emotion = StronglyPositive; Category = EObject Singular}; Weight = 1.0; Adjectives = Seq.empty<Adjective>}
        {Noun = {String= "Slytherin"; Article = NoArticle; Emotion = Positive; Category = EObject Singular}; Weight = 1.0; Adjectives = Seq.empty<Adjective> }
        {Noun = {String= "fandom"; Article = Sometimes "a"; Emotion = Positive; Category = EObject Singular}; Weight = 1.0; Adjectives = 
            [
                {String="Harry Potter";Emotion=id;Weight=1.0;Location=Before;Categories=Seq.empty<NounCategory>}] }
        {Noun = {String= "American masculinity culture"; Article = NoArticle; Emotion = Negative; Category = EObject Singular}; Weight = 1.0; Adjectives = Seq.empty<Adjective> }
        {Noun = {String= "yogurt"; Article = NoArticle; Emotion = Positive; Category = Object Singular}; Weight = 1.0; Adjectives = Seq.empty<Adjective>}

        {Noun = {String= "coffee"; Article = NoArticle; Emotion = Positive; Category = Object Singular}; Weight = 1.0; Adjectives = 
            [{String="single-origin";Emotion=id;Weight=1.0;Location=Before;Categories=Seq.empty<NounCategory>};
                {String="African";Emotion=id;Weight=1.0;Location=Before;Categories=Seq.empty<NounCategory>}]}
        {Noun = {String= "cats"; Article = NoArticle; Emotion = Positive; Category = Object Plural}; Weight = 1.0;  Adjectives = 
            [{String="cool";Emotion=id;Weight=1.0;Location=Before;Categories=Seq.empty<NounCategory>};]}
        {Noun = {String= "cat"; Article = Always "a"; Emotion = Positive; Category = Object Singular}; Weight = 1.0;  Adjectives = Seq.empty<Adjective>}
        {Noun = {String= "class"; Article = Always "the"; Emotion = Neutral; Category = Object Singular}; Weight = 1.0;  Adjectives = Seq.empty<Adjective>}
        {Noun = {String= "dog"; Article = Always "a"; Emotion = Positive; Category = Object Singular}; Weight = 1.0;  Adjectives = Seq.empty<Adjective>}

        {Noun = {String= "dogs"; Article = NoArticle; Emotion = Positive; Category = Object Plural}; Weight = 1.0;  Adjectives = Seq.empty<Adjective>}
        {Noun = {String= "bats"; Article = NoArticle; Emotion = Positive; Category = Object Plural}; Weight = 1.0;  Adjectives = 
            [{String="taxidermied";Emotion=id;Weight=1.0;Location=Before;Categories=Seq.empty<NounCategory>}] }
        {Noun = {String= "skirts"; Article = NoArticle; Emotion = Positive; Category = Object Plural}; Weight = 1.0;  Adjectives = Seq.empty<Adjective>}
        {Noun = {String= "snacks"; Article = NoArticle; Emotion = Positive; Category = Object Plural}; Weight = 1.0;  Adjectives = 
            [{String="sugary";Emotion=id;Weight=1.0;Location=Before;Categories=Seq.empty<NounCategory>};
             {String="sweet";Emotion=id;Weight=1.0;Location=Before;Categories=Seq.empty<NounCategory>}]}
        {Noun = {String= "gender"; Article = NoArticle; Emotion = Neutral; Category = EObject Singular}; Weight = 1.0;  Adjectives = Seq.empty<Adjective>}
        {Noun = {String= "dohickies"; Article = NoArticle; Emotion = Neutral; Category = Object Plural}; Weight = 1.0;  Adjectives= Seq.empty<Adjective>}
        {Noun = {String= "Tinder"; Article = NoArticle; Emotion = Positive; Category = EObject Singular}; Weight = 1.0; Adjectives = Seq.empty<Adjective> }
        {Noun = {String= "Gravity Falls"; Article = NoArticle; Emotion = Positive; Category = EObject Singular}; Weight = 1.0; Adjectives = Seq.empty<Adjective> }
        {Noun = {String= "Steven Universe"; Article = NoArticle; Emotion = Positive; Category = EObject Singular}; Weight = 1.0; Adjectives = Seq.empty<Adjective> }
        {Noun = {String= "Orphan Black"; Article = NoArticle; Emotion = Positive; Category = EObject Singular}; Weight = 1.0; Adjectives = Seq.empty<Adjective> }
        {Noun = {String= "80/20"; Article = NoArticle; Emotion = Positive; Category = EObject Singular}; Weight = 1.0; Adjectives = Seq.empty<Adjective> }
        {Noun = {String= "Hamilton"; Article = NoArticle; Emotion = Positive; Category = EObject Singular}; Weight = 1.0; Adjectives = Seq.empty<Adjective> }
        {Noun = {String= "Fun Home"; Article = NoArticle; Emotion = Positive; Category = EObject Singular}; Weight = 1.0; Adjectives = Seq.empty<Adjective> }
        {Noun = {String= "Hamilton Soundtrack"; Article = Always "the"; Emotion = Positive; Category = EObject Singular}; Weight = 1.0; Adjectives = Seq.empty<Adjective> }
        {Noun = {String= "Hamilton cast"; Article = Sometimes "the"; Emotion = Positive; Category = Person Plural}; Weight = 1.0; Adjectives = Seq.empty<Adjective> }
        {Noun = {String= "Meme Team"; Article = Sometimes "the"; Emotion = Positive; Category = Person Plural}; Weight = 1.0; Adjectives = Seq.empty<Adjective> }
        {Noun = {String= "Denver"; Article = NoArticle; Emotion = Positive; Category = Location}; Weight = 1.0; Adjectives = Seq.empty<Adjective> }
        {Noun = {String= "Seattle"; Article = NoArticle; Emotion = Positive; Category = Location}; Weight = 1.0; 
            Adjectives = [{String="rainy";Emotion=(fun x -> Negative);Weight=1.0;Location=Before;Categories=Seq.empty<NounCategory>};] }
        {Noun = {String= "LA"; Article = NoArticle; Emotion = Positive; Category = Location}; Weight = 1.0; Adjectives = Seq.empty<Adjective> }
        {Noun = {String= "Oklahoma"; Article = NoArticle; Emotion = Neutral; Category = Location}; Weight = 1.0; Adjectives = Seq.empty<Adjective> }
        {Noun = {String= "cheese"; Article = NoArticle; Emotion = Positive; Category = Object Singular}; Weight = 1.0; Adjectives = 
            [{String="melted";Emotion=id;Weight=1.0;Location=Before;Categories=Seq.empty<NounCategory>};
            {String="hot";Emotion=id;Weight=1.0;Location=Before;Categories=Seq.empty<NounCategory>}] }

        {Noun = {String= "Jonathan"; Article = NoArticle;Emotion = Positive; Category = Person Singular}; Weight = 1.0; Adjectives = 
            [{String="Adler";Emotion=id;Weight=3.0;Location=After;Categories=Seq.empty<NounCategory>}] }
        {Noun = {String= "Dana"; Article = NoArticle;Emotion = Positive; Category = Person Singular}; Weight = 0.75; Adjectives = Seq.empty<Adjective> }
        {Noun = {String= "Alex"; Article = NoArticle;Emotion = Positive; Category = Person Singular}; Weight = 0.5; Adjectives = Seq.empty<Adjective> }
        {Noun = {String= "Christian"; Article = NoArticle;Emotion = Positive; Category = Person Singular}; Weight = 0.5; Adjectives = Seq.empty<Adjective> }
        {Noun = {String= "Ari"; Article = NoArticle;Emotion = Positive; Category = Person Singular}; Weight = 0.5; Adjectives = Seq.empty<Adjective> }
        {Noun = {String= "Ali"; Article = NoArticle;Emotion = Positive; Category = Person Singular}; Weight = 0.5; Adjectives = Seq.empty<Adjective> }
        {Noun = {String= "Marianne"; Article = NoArticle;Emotion = Positive; Category = Person Singular}; Weight = 0.5; Adjectives = Seq.empty<Adjective> }
        {Noun = {String= "Lily"; Article = NoArticle;Emotion = Positive; Category = Person Singular}; Weight = 1.0; Adjectives = Seq.empty<Adjective> }
        {Noun = {String= "Nil"; Article = NoArticle;Emotion = StronglyPositive; Category = Person Singular}; Weight = 1.0; Adjectives =  Seq.empty<Adjective> }
        {Noun = {String= "Amanda"; Article = NoArticle;Emotion = StronglyPositive; Category = Person Singular}; Weight = 1.0; Adjectives =  Seq.empty<Adjective> }
        {Noun = {String= "Cait"; Article = NoArticle;Emotion = Positive; Category = Person Singular}; Weight = 1.0; Adjectives = Seq.empty<Adjective> }           
        {Noun = {String= "Kira"; Article = NoArticle;Emotion = Positive; Category = Person Singular}; Weight = 1.0; Adjectives = Seq.empty<Adjective> }           
        {Noun = {String= "Kacie"; Article = NoArticle;Emotion = Positive; Category = Person Singular}; Weight = 1.0; Adjectives = Seq.empty<Adjective> }         
        {Noun = {String= "Moofy"; Article = NoArticle;Emotion = Positive; Category = Person Singular}; Weight = 0.5; Adjectives = Seq.empty<Adjective> }
        {Noun = {String= "Eve"; Article = NoArticle;Emotion = Positive; Category = Person Singular}; Weight = 0.05; Adjectives = Seq.empty<Adjective> }           
        {Noun = {String= "Sparrow"; Article = NoArticle;Emotion = Positive; Category = Person Singular}; Weight = 0.05; Adjectives = Seq.empty<Adjective> }
        {Noun = {String= "Lilith"; Article = NoArticle;Emotion = Positive; Category = Person Singular}; Weight = 0.05; Adjectives = Seq.empty<Adjective> }
        {Noun = {String= "Cassian"; Article = NoArticle;Emotion = Positive; Category = Person Singular}; Weight = 0.05; Adjectives = Seq.empty<Adjective> }
        {Noun = {String= "Atreyu"; Article = NoArticle;Emotion = Positive; Category = Person Singular}; Weight = 0.05; Adjectives = Seq.empty<Adjective> }
        {Noun = {String= "Holly"; Article = NoArticle;Emotion = Positive; Category = Person Singular}; Weight = 1.0; Adjectives = Seq.empty<Adjective> }
        {Noun = {String= "Sunngyu"; Article = NoArticle; Emotion = Positive; Category = Person Singular}; Weight = 0.3; Adjectives = Seq.empty<Adjective>}
        {Noun = {String= "Woohyun"; Article = NoArticle; Emotion = Positive; Category = Person Singular}; Weight = 0.3; Adjectives = Seq.empty<Adjective>}
        {Noun = {String= "Hoya"; Article = NoArticle; Emotion = Positive; Category = Person Singular}; Weight = 0.3; Adjectives = Seq.empty<Adjective>}
        {Noun = {String= "Dongwoo"; Article = NoArticle; Emotion = Positive; Category = Person Singular}; Weight = 0.3; Adjectives = Seq.empty<Adjective>}
        {Noun = {String= "Infinite"; Article = NoArticle; Emotion = StronglyPositive; Category = Person Singular}; Weight = 1.0; Adjectives = Seq.empty<Adjective>}
        {Noun = {String= "Meme Team"; Article = NoArticle;Emotion = Positive; Category = Person Singular}; Weight = 1.0; Adjectives = Seq.empty<Adjective> }
        {Noun = {String= "ladies"; Article = NoArticle;Emotion = Positive; Category = Person Plural}; Weight = 1.0; Adjectives = 
            [{String="cute";Emotion=id;Weight=3.0;Location=Before;Categories=Seq.empty<NounCategory>};
            {String="single";Emotion=id;Weight=3.0;Location=After;Categories=Seq.empty<NounCategory>}] }
        {Noun = {String= "girls"; Article = NoArticle;Emotion = Positive; Category = Person Plural}; Weight = 1.0; Adjectives = 
            [{String="cute";Emotion=id;Weight=3.0;Location=Before;Categories=Seq.empty<NounCategory>}] }      
        {Noun = {String= "homeboy"; Article = NoArticle;Emotion = Negative; Category = Person Singular}; Weight = 1.0; Adjectives = Seq.empty<Adjective> }  
        {Noun = {String= "dude"; Article = NoArticle;Emotion = Negative; Category = Person Singular}; Weight = 0.8; Adjectives = Seq.empty<Adjective> }
        {Noun = {String= "dude bro"; Article = NoArticle;Emotion = Negative; Category = Person Singular}; Weight = 0.8; Adjectives = Seq.empty<Adjective> }
        {Noun = {String= "dude bros"; Article = NoArticle;Emotion = Negative; Category = Person Plural}; Weight = 0.8; Adjectives = Seq.empty<Adjective> }
        {Noun = {String= "dudes"; Article = NoArticle;Emotion = Negative; Category = Person Plural}; Weight = 0.8; Adjectives = 
            [{String="on the internet";Emotion=id;Weight=3.0;Location=After;Categories=Seq.empty<NounCategory>};
            {String="creepy";Emotion=id;Weight=3.0;Location=Before;Categories=Seq.empty<NounCategory>};] }            
        {Noun = {String= "buckos"; Article = NoArticle;Emotion = Negative; Category = Person Plural}; Weight = 0.8; Adjectives = Seq.empty<Adjective> }            
        {Noun = {String= "Kaiser"; Article = NoArticle;Emotion = Positive; Category = Pet Singular}; Weight = 1.0; Adjectives = Seq.empty<Adjective> }   
        {Noun = {String= "Lewis"; Article = NoArticle;Emotion = StronglyPositive; Category = Pet Singular}; Weight = 1.0; Adjectives = Seq.empty<Adjective> }   
        {Noun = {String= "ice cream"; Article = NoArticle;Emotion = StronglyPositive; Category = Object Singular}; Weight = 1.0; Adjectives = 
            [{String="Molly-Moon's";Emotion=(fun x -> Positive);Weight=1.0;Location=Before;Categories=Seq.empty<NounCategory>};] }   
        {Noun = {String= "cheese"; Article = NoArticle;Emotion = Positive; Category = Object Singular}; Weight = 1.0; Adjectives = Seq.empty<Adjective> }   
        {Noun = {String= "breakfast"; Article = NoArticle;Emotion = Positive; Category = Object Singular}; Weight = 1.0; Adjectives = Seq.empty<Adjective> }   
        {Noun = {String= "broccoli"; Article = NoArticle;Emotion = Negative; Category = Object Singular}; Weight = 1.0; Adjectives = Seq.empty<Adjective> }   
        {Noun = {String= "dyslexia"; Article = NoArticle;Emotion = Negative; Category = EObject Singular}; Weight = 1.0; Adjectives = Seq.empty<Adjective> }   
        {Noun = {String= "heteronormativity"; Article = NoArticle;Emotion = Negative; Category = EObject Singular}; Weight = 1.0; Adjectives = Seq.empty<Adjective> }   
        {Noun = {String= "rain"; Article = Sometimes "the";Emotion = Negative; Category = EObject Singular}; Weight = 1.0; Adjectives = Seq.empty<Adjective> }   
        {Noun = {String= "San Diego Comic Con"; Article = NoArticle;Emotion = Positive; Category = Event}; Weight = 1.0; Adjectives = Seq.empty<Adjective> }
        {Noun = {String= "Dragon Con"; Article = NoArticle;Emotion = Positive; Category = Event}; Weight = 1.0; Adjectives = Seq.empty<Adjective> }
        {Noun = {String= "Denver Comic Con"; Article = NoArticle;Emotion = Positive; Category = Event}; Weight = 1.0; Adjectives = Seq.empty<Adjective> }
        ] |> Seq.ofList
    let adjectives = 
        [
        {String="fucking";Emotion=id;Weight=1.0;Location=Before;Categories=[Object Singular; Object Plural; EObject Singular; EObject Plural; Location; Event]}
        {String="weird";Emotion=id;Weight=1.0;Location=Before;Categories=[Object Singular; Object Plural; EObject Singular; EObject Plural]}
        {String="mint-colored";Emotion=id;Weight=1.0;Location=Before;Categories=Seq.singleton (Object Singular)}
        {String="problematic";Emotion=(fun x -> Negative);Weight=1.0;Location=Before;Categories=Seq.singleton (Object Singular)}
        //{String="bananas";Emotion=None;Weight=1.0;Location=Before;Categories=allCategories}
        {String="great";Emotion=id;Weight=1.0;Location=Before;Categories=[Object Singular; Object Plural]}
        //{String="super";Emotion=boostEmotion;Weight=1.0;Location=Before;Categories=Seq.singleton (EObject Singular)}
        ] |> Seq.ofList

    let gerunds = 
        [
        {String = "blogging"; Emotion = id; Weight = 1.0}
        {String = "memeing"; Emotion = id; Weight = 1.0}
        {String = "gishing"; Emotion = id; Weight = 1.0}
        //{String = "crying"; Emotion = negateEmotion; Weight = 1.0}
        ]

    let gerundNoun (noun:Noun) =
        let gerund =
            gerunds
            |> Seq.map (fun x-> (x.Weight,x))
            |> pick
        {noun with String = noun.String + " " + gerund.String; Emotion = gerund.Emotion noun.Emotion}
        
    let combineNounAdjective (noun:Noun) (adjective:Adjective) =
        let emotion = adjective.Emotion noun.Emotion
        match adjective.Location with
            | Before -> {String=adjective.String + " " + noun.String;Emotion=emotion;Category=noun.Category; Article=noun.Article}
            | After -> {String=noun.String + " " + adjective.String;Emotion=emotion;Category=noun.Category; Article=noun.Article}


    let getAnyNoun () =
        let selectedNoun =
            nouns
            |> Seq.map (fun c -> (c.Weight,c))
            |> pick
        
        let genericAdjectiveProbability = 0.1
        let specificAdjectiveProbability = 0.5
        let gerundProbability = 0.0
        let applyAdjective (possibleAdjectives:Adjective seq) =
            if Seq.length possibleAdjectives > 0 then
                let totalAdjectiveProbability = possibleAdjectives
                                                |> Seq.map (fun adj -> System.Math.Exp(adj.Weight))
                                                |> Seq.sum
                let selectedAdjective =
                    possibleAdjectives
                    |> Seq.map (fun a -> (a.Weight,a))
                    |> pick
                combineNounAdjective selectedNoun.Noun selectedAdjective
            else selectedNoun.Noun
        if random.NextDouble() < genericAdjectiveProbability then
            let possibleAdjectives = 
                adjectives
                |> Seq.filter (fun (adjective:Adjective) -> Seq.exists (fun category -> category = selectedNoun.Noun.Category) adjective.Categories)
            applyAdjective possibleAdjectives
        else if random.NextDouble() < specificAdjectiveProbability then
            applyAdjective selectedNoun.Adjectives
        else if random.NextDouble() < gerundProbability then
            gerundNoun selectedNoun.Noun
        else selectedNoun.Noun
    let getNounWithFilters (emotions:Emotion seq) (categories: NounCategory seq) =
        let emotionSet = Set.ofSeq emotions
        let categorySet = Set.ofSeq categories

        Seq.initInfinite id
        |> Seq.map (fun idx -> (idx,getAnyNoun()))
        |> Seq.find (fun (idx,noun) -> idx > 50 || (Set.contains noun.Emotion emotionSet) && (Set.contains noun.Category categorySet))
        |> snd

    let repeater (parameter: float) (s:string) =
        //let parameter = 0.2
        let times = System.Math.Log(random.NextDouble()) / System.Math.Log(1.0-parameter)
                    |> System.Math.Floor
                    |> int
                    |> max 1
        {1..times}
        |> Seq.map (fun i -> s)
        |> Seq.fold (+) ""
    let nString (item:Noun) =
        (match item.Article with | Always a -> a + " " | _ -> "") + item.String
    let nWithAString (item:Noun) =
        (match item.Article with | Always a | Sometimes a -> a + " " | _ -> "") + item.String

    let generateSentenceWithFilters (emotions:Emotion seq) (sentence:Sentence)  =
        let emotionSet = Set.ofSeq emotions
        Seq.initInfinite id
        |> Seq.map (fun idx -> (idx,sentence.Generator()))
        |> Seq.find (fun (idx,(sentence,emotion)) -> idx > 50 || (Set.contains emotion emotionSet))
        |> snd

    let baseSentences = 
        [
        {Generator=(fun () -> ((nWithAString (gerundNoun (getNounWithFilters [StronglyPositive] [Person Singular; Pet Singular]))) + " is a photo I want on my body."),StronglyPositive);Weight=1.0}
        {Generator=(fun () -> ("I fucking love " + nString (getNounWithFilters [Positive; StronglyPositive] allCategories) + ".",StronglyPositive));Weight=1.0}
        {Generator=(fun () -> 
            let item1 = getNounWithFilters [Positive; StronglyPositive] [Object Singular]
            let item2 = Seq.initInfinite (fun x -> getNounWithFilters [Positive; StronglyPositive] [Object Singular])
                        |> Seq.skipWhile (fun x -> x = item1)
                        |> Seq.head
            ("a sign for " + nString item1+ " and " + nString item2 + " would be a trap for a Heather in a cartoon.",StronglyPositive));Weight=1.0}
        {Generator=(fun () -> 
            let item = getNounWithFilters [Positive; StronglyPositive] allCategories
            (("have I told you about " + nString (item)) + "?"),item.Emotion);
        Weight=1.0}
        {Generator=(fun () -> 
            let item = (getNounWithFilters [Positive; StronglyPositive] [Object Singular])
            (((nString item) + " is my jam."),item.Emotion));
        Weight=1.0}
        {Generator=(fun () -> 
            let item = getNounWithFilters allEmotions allSingularCategories
            (((nString item) + " is absolutely bonkers."),boostEmotion item.Emotion));
        Weight=1.0}
        {Generator=(fun () -> 
            let item = getNounWithFilters allEmotions allPluralCategories
            ((nString item) + " are absolutely bonkers.", boostEmotion item.Emotion));
        Weight=0.2}
        {Generator=(fun () -> 
            let item = getNounWithFilters allEmotions [Object Singular]
            (((nString item) + " is cuckoo bananas."),item.Emotion));
        Weight=1.0}
        {Generator=(fun () -> 
            let item = getNounWithFilters allEmotions [Object Plural]
            ((nString item) + " are cuckoo bananas.",boostEmotion item.Emotion));
        Weight=0.4}
        {Generator=(fun () -> 
            let item = getNounWithFilters [Negative; StronglyNegative] [Object Singular]
            ((nString item + " you wanna rumble?"), item.Emotion));
        Weight=1.0}
        {Generator=(fun () -> 
            let item = getNounWithFilters [Positive;StronglyPositive] [Person Singular;Pet Singular; Person Plural; Pet Plural]
            (("I love " + nWithAString item + " s"+  (repeater 0.2 "o") + " much."), StronglyPositive));
        Weight=1.0}
        {Generator=(fun () -> ((nString (getNounWithFilters [Positive;StronglyPositive] [Person Singular;Pet Singular; Person Plural; Pet Plural]) + " is s"+  (repeater 0.2 "o") + " nice."),StronglyPositive));Weight=1.0}
        {Generator=(fun () -> (((nString (getNounWithFilters [Positive;StronglyPositive] [Person Singular])) + " is the coolest person I know."),Positive)) ;Weight=1.0}
        {Generator=(fun () -> (((nString (getNounWithFilters [Positive;StronglyPositive] [Person Singular])) + " is a GEM."),Positive)) ;Weight=1.0}
        {Generator=(fun () -> (("I miss " + (nString (getNounWithFilters [Positive;StronglyPositive] [Person Singular; Person Plural])) + " s" + (repeater 0.2 "o")+ " much."),Positive)) ;Weight=1.0}
        {Generator=(fun () -> ((nString (getNounWithFilters [Positive;StronglyPositive] [Object Singular;EObject Singular]) + " is the bomb-diggity."),Positive));Weight=0.5}
        {Generator=(fun () -> ((nString (getNounWithFilters [Positive;StronglyPositive] [Object Singular;EObject Singular]) + " is my favorite."),Positive));Weight=0.5}
        {Generator=(fun () -> ((nString (getNounWithFilters [Positive;StronglyPositive] [Object Plural;EObject Plural]) + " are the bomb-diggity."),Positive));Weight=0.3}
        {Generator=(fun () -> ((nString (getNounWithFilters [Positive;StronglyPositive] [Object Plural;EObject Plural]) + " are my favorite."),Positive));Weight=0.3}
        {Generator=(fun () -> (nString (getNounWithFilters [Positive;StronglyPositive] allCategories) + "? In 1000%.",Positive));Weight=1.0}
        {Generator=(fun () -> 
            let item = getNounWithFilters [Positive;StronglyPositive] allCategories
            (("how do you feel about " + (nString item) + "?",item.Emotion)));
        Weight=1.0}
        {Generator=(fun () -> 
            ("does anyone have " +  (nWithAString (getNounWithFilters allEmotions [Object Singular])) + "? I need it for Gishwhes.",Neutral));
        Weight=0.7}
        {Generator=(fun () -> 
            ("does anyone have " +  (nWithAString (getNounWithFilters allEmotions [Object Plural])) + "? I need them for Gishwhes.",Neutral));
        Weight=0.7}
        {Generator=(fun () -> (("I am here for " + (nString (getNounWithFilters [Positive;StronglyPositive] [EObject Singular;Object Singular; EObject Plural; Object Plural]))+ ".",Positive)));Weight=1.0}
        {Generator=(fun () -> 
            let item = getAnyNoun()
            (("holy crackers you wouldn't believe this post on " + (convertToTumblr item.String) + ".tumblr.com."),boostEmotion item.Emotion));
        Weight=1.0}
        {Generator=(fun () -> (("I'm so tired of " + (nWithAString (getNounWithFilters [Negative;StronglyNegative] [EObject Singular;Object Singular; EObject Plural; Object Plural]))+ ".",Negative)));Weight=1.0}
        {Generator=(fun () -> (("I am done with " + (nWithAString (getNounWithFilters [Negative;StronglyNegative] [EObject Singular;Object Singular; EObject Plural; Object Plural]))+ ".",Negative)));Weight=1.0}
        {Generator=(fun () -> ((nWithAString (getNounWithFilters [Negative;StronglyNegative] [EObject Singular; EObject Plural]))+ " is a thing I am OVER.",StronglyNegative));Weight=1.0}
        {Generator=(fun () -> (nString (getNounWithFilters [Positive;StronglyPositive] [Person Singular])) + " should move with me to " + (nString (getNounWithFilters [Positive;StronglyPositive] [Location])) + ".",Positive) ;Weight=1.0}
        {Generator=(fun () -> ("I'm so excited about " + (nString (getNounWithFilters [Positive;StronglyPositive] allCategories)) + " I could punch a man.",Positive));Weight=0.7}
        {Generator=(fun () -> ("I'm so excited about " + (nString (getNounWithFilters [Positive;StronglyPositive] allCategories)) + " I could punch a dragon.",Positive));Weight=0.7}
        {Generator=(fun () -> 
            let person = getNounWithFilters allEmotions [Person Singular]
            ((nString person) + " is going to be at " + (nString (getNounWithFilters allEmotions [Event])) + ".", person.Emotion));Weight=1.0}
        {Generator=(fun () -> 
            let person = getNounWithFilters allEmotions [Person Plural]
            ((nString person) + " are going to be at " + (nString (getNounWithFilters allEmotions [Event])) + ".", person.Emotion));Weight=1.0}
        {Generator=(fun () -> 
            let person = getNounWithFilters allEmotions [Person Singular; Pet Singular]
            ((nString person) + " is here for the party.", person.Emotion));Weight=1.0}
        {Generator=(fun () -> 
            let people = getNounWithFilters allEmotions [Person Plural]
            let item = getNounWithFilters [Positive;StronglyPositive] [Object Singular; Object Plural; EObject Singular; EObject Plural]
            ("finally, " + (nWithAString people) + " who know what I want: " + (nString item) + ".", Positive));Weight=1.0}
            //I'm not about that life?
        ] 
        |> Seq.ofList


    let sentenceAdjusters =
        [
        {Adjuster = (fun s -> {s with 
                                Generator = (fun () -> 
                                    let baseSentence = s.Generator()
                                    ("it's almost like " + (fst baseSentence),snd baseSentence))}); 
        Weight = 1.0}
        {Adjuster = (fun s -> {s with 
                                Generator = (fun () -> 
                                    let baseSentence = s.Generator()
                                    ("holy crackers " + fst baseSentence,snd baseSentence))});
        Weight = 1.0}
        {Adjuster = (fun s -> {s with Generator = (fun () -> 
                                    let baseSentence = s.Generator()
                                    ("oh my god, " + fst baseSentence, snd baseSentence))});
        Weight = 1.0}
        {Adjuster = (fun s -> {s with Generator = (fun () ->
                                    let baseSentence = s.Generator()
                                    ("holy shit " + fst baseSentence, snd baseSentence))});
        Weight = 1.0}
        {Adjuster = (fun s -> {s with Generator = (fun () ->
                                    let baseSentence = s.Generator()
                                    ("yo " + fst baseSentence, snd baseSentence))});
        Weight = 1.0}
        {Adjuster = (fun s -> {s with Generator = (fun () -> 
                                    let baseSentence = s.Generator()
                                    ("this is dumb but " + fst baseSentence, snd baseSentence))});
        Weight = 1.0}
        {Adjuster = (fun s -> {s with Generator = (fun () -> 
                                    let baseSentence = s.Generator()
                                    ("I told you, " + fst baseSentence, snd baseSentence))});
        Weight = 1.0}
        {Adjuster = (fun s -> {s with Generator = (fun () -> 
                                    let baseSentence = s.Generator()
                                    ("okay so " + fst baseSentence, snd baseSentence))});
        Weight = 1.0}
        {Adjuster = (fun s -> {s with Generator = (fun () -> 
                                    let baseSentence = s.Generator()
                                    ("god let me talk about " + fst baseSentence, snd baseSentence))});
        Weight = 1.0}
        {Adjuster = (fun s -> {s with Generator = (fun () -> 
                                    let baseSentence = s.Generator()
                                    ("what I'm saying is " + fst baseSentence, snd baseSentence))});
        Weight = 1.0}
        {Adjuster = (fun s -> {s with Generator = (fun () -> 
                                    let baseSentence = s.Generator()
                                    ("so, " + fst baseSentence, snd baseSentence))});
        Weight = 1.0}
        {Adjuster = (fun s -> {s with Generator = (fun () -> 
                                    let baseSentence = s.Generator()
                                    ("you're wrong and here's why: " + fst baseSentence, snd baseSentence))});
        Weight = 1.0}
        {Adjuster = (fun s -> {s with Generator = (fun () -> 
                                    let baseSentence = s.Generator()
                                    ("I'm squarely here today: " + fst baseSentence, snd baseSentence))});
        Weight = 1.0}

        {Adjuster = (fun s -> {s with Generator = (fun () -> 
                                    let baseSentence = s.Generator()
                                    ("PSA " + fst baseSentence, snd baseSentence))});
        Weight = 1.0}
        {Adjuster = (fun s -> {s with Generator = (fun () -> 
                                    let baseSentence = s.Generator()
                                    ("yo " + fst baseSentence, snd baseSentence))});
        Weight = 1.0}
        {Adjuster = (fun s -> {s with Generator = (fun () -> 
                                    let baseSentence = s.Generator()
                                    ("scenario: " + fst baseSentence, snd baseSentence))});
        Weight = 1.0}
        {Adjuster = (fun s -> {s with Generator = (fun () -> 
                                    let baseSentence = s.Generator()
                                    ("hold on: " + fst baseSentence, snd baseSentence))});
        Weight = 1.0}
        {Adjuster = (fun s -> {s with Generator = (fun () -> 
                                    let baseSentence = s.Generator()
                                    (fst baseSentence + " Whoops.", snd baseSentence))});
        Weight = 1.0}
        {Adjuster = (fun s -> {s with Generator = (fun () -> 
                                    let baseSentence = generateSentenceWithFilters [Positive; StronglyPositive] s
                                    (fst baseSentence + " What a world.", snd baseSentence))});
        Weight = 1.0}
        {Adjuster = (fun s -> {s with Generator = (fun () -> 
                                    let baseSentence = s.Generator()
                                    (fst baseSentence + " " + (repeater 0.2 "❤️❤️") + ".", snd baseSentence))});
        Weight = 1.0}
//        {Adjuster = (fun s -> {s with Generator = (fun () -> 
//                                    let baseSentence = s.Generator()
//                                    (fst baseSentence + " I KN" + (repeater 0.2 "0") + "W.", snd baseSentence))});
//        Weight = 1.0}
        {Adjuster = (fun s -> {s with Generator = (fun () -> 
                                    let baseSentence = generateSentenceWithFilters [Negative; StronglyNegative] s
                                    (fst baseSentence + " 💅", snd baseSentence))});
        Weight = 1.0}
        {Adjuster = (fun s -> {s with Generator = (fun () -> 
                                    let baseSentence = generateSentenceWithFilters [Positive; StronglyPositive] s
                                    (fst baseSentence + " " + (repeater 0.2 "💔❤️") + ".", snd baseSentence))});
        Weight = 1.0}
        {Adjuster = (fun s -> {s with Generator = (fun () -> 
                                    let baseSentence = generateSentenceWithFilters [Positive; StronglyPositive] s
                                    ("in a dream world " + fst baseSentence, snd baseSentence))});
        Weight = 1.0}
        {Adjuster = (fun s -> {s with Generator = (fun () -> 
                                    let baseSentence = generateSentenceWithFilters [Positive; StronglyPositive] s
                                    (fst baseSentence +  " YE" + (repeater 0.2 "S") + ".", snd baseSentence))});
        Weight = 1.0}
        {Adjuster = (fun s -> {s with Generator = (fun () -> 
                                    let baseSentence = s.Generator()
                                    (fst baseSentence + " Seriously.", snd baseSentence))});
                                    Weight = 1.0}
        {Adjuster = (fun s -> {s with Generator = (fun () -> 
                                    let baseSentence = s.Generator()
                                    (fst baseSentence + " You played yourself.", snd baseSentence))});
        Weight = 1.0}
        {Adjuster = (fun s -> {s with Generator = (fun () -> 
                                    let baseSentence = s.Generator()
                                    (fst baseSentence + " That's me forever.", snd baseSentence))});
        Weight = 1.0}
        //{Adjuster = (fun s -> {s with Generator = (fun () -> s.Generator() + " tbh.")}); Weight = 1.0}
        //{Adjuster = (fun s -> {s with Generator = (fun () -> "if you don't think that " + s.Generator() + ", then you're mistaken")}); Weight = 1.0}
        //{Adjuster = (fun s -> {s with Generator = (fun () -> s.Generator() + " NICE.")}); Weight = 1.0}
        //{Adjuster = (fun s -> {s with Generator = (fun () -> s.Generator() + " CUTE.")}); Weight = 1.0}
        ]
    //MAKE HEATHERBOT REPLY TO MESSAGES TO HER
    let capitalizeFirst (input:string) : string =
        if (System.String.IsNullOrEmpty(input)) then input
        else input.Substring(0,1).ToUpper() + input.Substring(1);

    let getAnySentence () = 
        baseSentences
        |> Seq.map (fun s -> (s.Weight,s))
        |> pick
    let runSentence () = 
        getAnySentence ()
        |> (fun sentence -> 
            if random.NextDouble() < sentenceAdjusterProbability then 
                sentenceAdjusters
                |> Seq.map (fun s -> (s.Weight,s))
                |> pick
                |> (fun adjust -> adjust.Adjuster sentence)
            else sentence)
        |> (fun sentence -> sentence.Generator ())
        |> fst
        |> capitalizeFirst
        

module Program =
    [<EntryPoint>]
    let main argv = 
        do Twitter.setConfig "config.json" |> ignore
        let tweets = Twitter.getTweets (argv.[0])
        let generator = Process.convertTweetsToGenerator tweets
        let tweet = Process.runMarkovChain generator
        printfn "%s" tweet
        0 // return an integer exit code

