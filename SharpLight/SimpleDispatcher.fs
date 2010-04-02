module SharpLight.Core

open System
open  Microsoft.FSharp.Collections
open LazyList
open Html
open System.Web
open Reader
let flip f = fun b  a-> f a b
type 'a Endo= 'a -> 'a
type Servlet=  Reader<Request,Response>
and Url= String
and Request= {Request:Web.HttpRequest;UrlParts:string LazyList;Method:Method;AcceptedMime: string seq}
and Method= Get|Post|Put|Unsupported
and Response ={ Status:int*string;MimeType:string; Content:char seq}

type Matcher = abstract member Do: (Request->  Servlet Option)
               abstract member Info:String

and 'a ParamMatcher='a->Matcher

let matchIt rq (matchers:Matcher list)=  [for m in matchers -> m.Do]|> Seq.tryPick ((|>) rq) 

[<AbstractClass>]
type LightController() = 
           abstract member Matchers : Matcher list
           interface  System.Web.IHttpHandler with
                member x.ProcessRequest context= 
                                let makeRequest r=   {Request=r; 
                                                      UrlParts= (r.Path.Trim [|'/'|]) .Split [|'/'|] |> LazyList.ofArray
                                                      Method=match r.HttpMethod with |"GET"->Get |"Post" ->Post|_-> Unsupported;
                                                      AcceptedMime=r.AcceptTypes}
                                let request=makeRequest context.Request 
                                let   matched=matchIt request x.Matchers                                                          
                                in match matched with
                                               |None-> context.Response.StatusCode <- 404
                                               |Some servlet -> Seq. iter ( fun c -> context.Response.Write (c:char)) ((runReader servlet request).Content)
                member x.IsReusable= true
///Need to test how effecient this function is
///


let splitAt n xs=(LazyList.take n xs,LazyList.skip n xs)

let dir (dir:string)(subs :Matcher  list) : Matcher=  
           let dirSplitted=(LazyList.ofArray <| dir.Split('/'))
           in {new Matcher with
             member x.Do=fun rq->let concerned,rest= splitAt (LazyList.length dirSplitted) rq.UrlParts
                                 in if  dirSplitted = concerned then matchIt  {rq with UrlParts= rest} subs 
                                    else None
             member x.Info= "not documented"  }

let any (s : Servlet) :Matcher= {new Matcher with member x.Do= fun _->Some s
                                                  member x.Info= "Any"}

let mime_types ms s:Matcher= {new Matcher with 
                                     member x.Do=fun rq-> let doesMatch=  Seq.exists2  (=)  ms rq.AcceptedMime 
                                                          in if doesMatch then Some s else None
                                     member x.Info= String.concat " + " ms}

let webMethod m s : Matcher= {new Matcher with 
                                     member x.Do =fun rq -> if rq.Method = m then Some s else None
                                     member x.Info= m.ToString() }
let web_get s= webMethod Get s
let web_post s= webMethod Post s

let webMethod1 m (subs :Matcher  list) ={new Matcher with
                                                member x.Do = fun(rq) -> if rq.Method = m then matchIt rq subs else None
                                                member x.Info= m.ToString()}


type ComposableServlet= Reader<Request,Response Endo>
//hacky
let (-|) :(ComposableServlet)-> (ComposableServlet) -> ComposableServlet= (>>>)

let yield_string (s:string):ComposableServlet=  reader{return fun res -> {res with Content=Seq.append res.Content <| s.ToCharArray()}}                                                                    
                                                         
let mime t :ComposableServlet = reader{return fun res -> {res with MimeType=t}}
let code code :ComposableServlet=reader{return fun res ->{res with Status=(code,"")}}
let ok:ComposableServlet = code 200 
let text_html= "text/html"
let no_content (s:string)=code 204  -| mime text_html -| yield_string  s


let yield_html (html:Html)= mime text_html -| yield_string (html |> to_s) 
                                                       
//let yield_stream  (s:IO.Stream):Servlet= fun res  _ -> {res with  Content= s.


let from_data f ts : ComposableServlet= reader {let! r= asks f in return! ts r}

let blank= returnR {Response.Status=(200,"");MimeType="";Content=Seq.empty}                                         
let doNothing=ok <*>  blank

let combination= dir "products" [dir "old"
                               [webMethod Get doNothing;
                                webMethod Post (  (ok >>> (yield_string "post on products/old") <*> blank))]] 
                  
type SimpleDispatcher= inherit LightController override x.Matchers=[combination]             