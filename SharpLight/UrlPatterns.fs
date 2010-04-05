module UrlPatterns
open System.Text.RegularExpressions
let t2 a b=(a,b)
let matchUrl pattern:string->(Map<string,string>* string ) Option=
            let regex=(new Regex(pattern))
            let names= regex. GetGroupNames()
            in fun url -> let m=regex.Match(url)
                          if not(m.Success) then None
                          else let groups= m.Groups in
                               let paramMap=[for i in 0..groups.Count do 
                                             let key= regex.GroupNameFromNumber(i)
                                             if not <| System.String.IsNullOrEmpty key 
                                             then yield (regex.GroupNameFromNumber(i), groups.[i].Value)] |> Map.ofList 
                               in Some(paramMap,defaultArg <| paramMap.TryFind "rest" <| "") 

let urlParams= matchUrl "Show/(?<topics>.*)/(?<id>[\\d]+)/(?<title>[-\\w]+)/(?<rest>.*)" "http://localhost/Show/Topic/SubTopic/SubSubTopic/123/This-is-an-example/rest/evenmore/tomatch/later"

