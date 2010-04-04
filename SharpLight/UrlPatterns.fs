module UrlPatterns
open System.Text.RegularExpressions
let matchUrl pattern:string->Map<string,string> Option=
            let regex=(new Regex(pattern))
            let names= regex. GetGroupNames()
            in fun url -> let m=regex.Match(url)
                          if not(m.Success) then None
                          else let groups= m.Groups in
                               [for i in 0..groups.Count do 
                                let key= regex.GroupNameFromNumber(i)
                                if not <| System.String.IsNullOrEmpty key 
                                then yield (regex.GroupNameFromNumber(i), groups.[i].Value)] |> Map.ofList |> Some

let urlParams= matchUrl "Show/(?<topics>.*)/(?<id>[\\d]+)/(?<title>.*)" "http://localhost/Show/Topic/SubTopic/SubSubTopic/123/This-is-an-example"

