#light

namespace MvcFSharp

open System
open System.Web.UI

type _Default() =
  inherit Page()
  
  member x.Page_Load(sender:obj, e:EventArgs) =
    x.Response.Redirect("~/Home.mvc/Index")