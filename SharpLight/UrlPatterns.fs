module UrlPatterns
open System.Text.RegularExpressions
open FunUtils
open Maybe
let t2 a b=(a,b)

let matchAndExtractParams (regex:Regex)=let names= regex. GetGroupNames()
                                        in fun url -> let m=regex.Match(url)
                                                      if not(m.Success) then None
                                                      else let groups= m.Groups 
                                                           in[for i in 0..groups.Count do 
                                                              let key= regex.GroupNameFromNumber(i)
                                                              if not <| System.String.IsNullOrEmpty key 
                                                              then yield (regex.GroupNameFromNumber(i), groups.[i].Value)] |> Map.ofList |> Some
let matchXUrl pattern :string->(Map<string,string>* string ) Option=
    let regex=(new Regex(pattern))
    let names= matchAndExtractParams regex
    in fun url -> let map=names url
                  in Option.map (fun paramMap -> (paramMap,defaultArg <| paramMap.TryFind "rest" <| "")) map

let urlParams= matchXUrl "Show/(?<topics>.*)/(?<id>[\\d]+)/(?<title>[-\\w]+)/(?<rest>.*)" "http://localhost/Show/Topic/SubTopic/SubSubTopic/123/This-is-an-example/rest/evenmore/tomatch/later"

let matchUrl pattern =
    let simpleTemplateRegex=new Regex "{([-\\w]+)}" in
    let final=simpleTemplateRegex.Replace(pattern,
                 fun (m:Match) -> let trimmed=m.Value.Trim([|'{';'}'|])in 
                                  if trimmed="_" then "(?<rest>.*)"
                                  else System.String.Format("(?<{0}>[-\\w]+)",trimmed) )
    in matchXUrl final


let result= matchUrl "Show/{topics}/{id}/{title}/{_}" "http://localhost/Show/Topic/123/This-is-an-example/rest/evenmore/tomatch/later"
let result1= matchUrl "Show/{topics}/{id}/{title}/{_}/{later}" "http://localhost/Show/Topic/123/This-is-an-example/rest/evenmore/tomatch/later"

let lazySplit s c=LazyList.unfold (fun (s:string) -> match s.Split([|c|],2) with |[|""|] -> None
                                                                                 |[|a|] -> Some(a,"")
                                                                                 |[|a;b|] -> Some (a,b)                                                                                 
                                                                                 |_->  None) s

let rec foldr (f: 'a -> 's Lazy-> 's  ) z list=
      match list with LazyList.Cons(x,xs)-> f x (lazy(foldr f z xs))
                      |LazyList.Nil -> z 
                        
let concatMap m1 m2= m1

// This function looks very hacky and buggy for me, I have to come back to it
let matchUrlSectionedNoGreedy (pattern:string) = 
 let simpleTemplateRegex=new Regex "{([-\\w]+)}"
 let patternInRegex=simpleTemplateRegex.Replace(pattern,
                      fun (m:Match) -> let trimmed=m.Value.Trim([|'{';'}'|])in 
                                        System.String.Format("(?<{0}>[-\\w]+)",trimmed) )
 let patternSplitted=LazyList.map (fun p -> new Regex(p)) (lazySplit patternInRegex '/')
 fun (url:string)->
    let dirSplitted=lazySplit url '/'     
    if(LazyList.length dirSplitted < LazyList.length patternSplitted) then None
    else let concerned,rest= splitAt (LazyList.length patternSplitted)  dirSplitted
         let matched= LazyList.map2 ( matchAndExtractParams) patternSplitted concerned
         let result= foldr (fun m previous -> maybe{let! a=m 
                                                    let! p= previous.Value
                                                    return concatMap  p m }) (Some Map.empty) matched 
         in Option.map (fun m -> (m,String.concat "/" rest)) result
            
