#r "..//Dependencies//FSharp.PowerPack.dll"
#load "Maybe.fs" "FunUtils.fs" "Html.fs" "Reader.fs" "UrlPatterns.fs"
#load "SimpleDispatcher.fs"

open SharpLight.Core
open Reader

let combination= dir "products" [dir "old"
                               [webMethod Get doNothing;
                                webMethod Post (blank  |*> ok *>> yield_string "post on products/old" *>> toByteResponse  )]] 

