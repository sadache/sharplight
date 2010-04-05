module UrlPatterns
open System.Text.RegularExpressions
let t2 a b=(a,b)
let matchXUrl pattern :string->(Map<string,string>* string ) Option=
    let regex=(new Regex(pattern))
    let names= regex. GetGroupNames()
    in fun url -> let m=regex.Match(url)
                  if not(m.Success) then None
                  else let groups= m.Groups
                       let paramMap=[for i in 0..groups.Count do
                                     let key= regex.GroupNameFromNumber(i)
                                     if not <| System.String.IsNullOrEmpty key
                                     then yield (regex.GroupNameFromNumber(i), groups.[i].Value)] |> Map.ofList
                       in Some(paramMap,defaultArg <| paramMap.TryFind "rest" <| "")

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
