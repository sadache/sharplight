#light

namespace MvcFSharp

open System.Web.Mvc
open System.Web.Routing
open MvcContrib.ControllerFactories
open MvcContrib.NHamlViewEngine

type MvcConstraint2 = { action:string; id:string }
type MvcConstraint3 = { controller:string; action:string; id:string }

type MvcApplication() =
  inherit System.Web.HttpApplication()
 
  static member RegisterRoutes(routes:RouteCollection) =
    routes.Add(
      new Route("{controller}.mvc/{action}/{id}", 
        new MvcRouteHandler(), 
        Defaults = new RouteValueDictionary({ new MvcConstraint2 with action = "index" and id = "" })))
    
    routes.Add(
      new Route("Default.aspx", 
        new MvcRouteHandler(), 
        Defaults = new RouteValueDictionary({ controller = "Home"; action = "index"; id = "" })))
 
  member x.Application_Start() =
    MvcApplication.RegisterRoutes RouteTable.Routes
    ViewEngines.Engines.Add(new NHamlViewFactory())
    
    
    
    