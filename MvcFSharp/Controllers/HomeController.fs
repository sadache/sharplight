#light

namespace MvcFSharp.Controllers

open MvcFSharp.Models
open System.Web.Mvc

[<HandleError>]
type HomeController() =
  inherit Controller()
  
  member x.Index() =
    x.View("Index")
    
  member x.About() =
    x.View("About")
    
  member x.Numbers() =
    let viewData = { Numbers = {1..10} }
    x.View("Numbers", viewData)
