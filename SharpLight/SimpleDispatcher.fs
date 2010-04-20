module SharpLight.Core
open FunUtils
open System
open Microsoft.FSharp.Collections
open LazyList
open Html
open System.Web
open Reader

type Url= String
type  Request= {Request:Web.HttpRequest;UrlParts:string LazyList;Method:Method;AcceptedMime: string seq;UrlParams:Map<string,string>}
and   Method= Get|Post|Put|Unsupported
type ResponseMeta={ Status:int*string;MimeType:string}
type 'a Response =ResponseMeta * 'a
type 'a Servlet=  Reader<Request,'a  Response>
type ToStream= abstract member Get: Byte seq
type Matcher = abstract member Do: (Request->  Byte seq Servlet Option)
               abstract member Info:String

and 'a ParamMatcher='a->Matcher

let matchIt rq (matchers:Matcher list)=  [for m in matchers -> m.Do]|> Seq.tryPick ((|>) rq) 

[<AbstractClass>]
type LightController() = 
           abstract member Matchers : Matcher list
           interface  System.Web.IHttpHandler with
                member x.ProcessRequest context= 
                        let makeRequest r=   {Request=r; 
                                              UrlParams=Map.empty;
                                              UrlParts= (r.Path.Trim [|'/'|]) .Split [|'/'|] |> LazyList.ofArray
                                              Method=match r.HttpMethod with |"GET"->Get |"Post" ->Post|_-> Unsupported;
                                              AcceptedMime=r.AcceptTypes}
                        let request=makeRequest context.Request 
                        let   matched=matchIt request x.Matchers                                                          
                        in match matched with
                               |None-> context.Response.StatusCode <- 404
                               |Some servlet -> Seq.iter ( fun c -> context.Response.OutputStream.WriteByte (c:byte)) <| snd (runReader servlet request)
                member x.IsReusable= true

///Need to test how effecient this function is
///


type urlParams=Map<string,string> 
//TODO I might need a State monad here!
//matchers
let dir (dir:string)(subs :Matcher  list) : Matcher=  
       let dirSplitted=(LazyList.ofArray <| dir.Split('/'))
       in {new Matcher with
         member x.Do=fun rq->let concerned,rest= splitAt (LazyList.length dirSplitted) rq.UrlParts
                             in if  dirSplitted = concerned then matchIt  {rq with UrlParts= rest;} subs 
                                else None
         member x.Info= "not documented"  }

let any (s : Servlet<_>) :Matcher= {new Matcher with member x.Do= fun _->Some s
                                                     member x.Info= "Any"}

let mime_types ms s:Matcher= 
    {new Matcher with 
         member x.Do=fun rq-> let doesMatch=  Seq.exists2  (=)  ms rq.AcceptedMime 
                              in if doesMatch then Some s else None
         member x.Info= String.concat " + " ms}

let webMethod m s : Matcher= 
    {new Matcher with 
         member x.Do =fun rq -> if rq.Method = m then Some s else None
         member x.Info= m.ToString() }

let web_get s= webMethod Get s
let web_post s= webMethod Post s



let webMethod1 m (subs :Matcher  list) =
    {new Matcher with
        member x.Do = fun(rq) -> if rq.Method = m then matchIt rq subs else None
        member x.Info= m.ToString()}

//composable partial responses
type ComposableServlet<'a,'b>= Reader<Request,'a Response -> 'b Response>
//hacky
let ll :_-> _ -> ComposableServlet<'a,'b>= (>>>)

let yield_string (s:string):ComposableServlet<_,Char seq>=  reader{return fun (meta,_) -> (meta,Seq.ofArray <| s.ToCharArray())}                                                                    
                                                         
let mime t :ComposableServlet<'a,'a> = reader{return fun (meta,content) -> {meta with MimeType=t},content}
let code code :ComposableServlet<'a,'a>=reader{return fun (meta,content) ->{meta with Status=(code,"")},content}
let ok<'a> :ComposableServlet<'a,'a>  = code 200 
let text_html= "text/html"
let no_content (s:string)=code 204  >>> mime text_html >>> yield_string  s


let yield_html (html:Html)= mime text_html >>> yield_string (html |> to_s) 
                                                       
//let yield_stream  (s:IO.Stream):Servlet= fun res  _ -> {res with  Content= s.

//using data in Request object
//I need to define more useful composable data functions
let from_data f ts : ComposableServlet<'a,'b>= reader {let! r= asks f in return! ts r}

let blank<'a>= returnR ({ResponseMeta.Status=(200,"");MimeType="";},Seq.empty) 
let somethingElse= ok <*>  returnR ({ResponseMeta.Status=(200,"");MimeType="";},"vxvxc")                                           
let doNothing<'a>=ok <*>  blank


let getByte (c:char):byte array= ( System.Text.Encoding.UTF8.GetBytes([|c|])) 
//really horrible!
let toByteResponse= reader{return fun (meta,chars:Char seq)-> (meta, Seq.concat <| Seq.map getByte chars)}

let combination= dir "products" [dir "old"
                               [webMethod Get doNothing;
                                webMethod Post (ok >>> yield_string "post on products/old" >>> toByteResponse <*> blank)]] 
                  
type SimpleDispatcher= inherit LightController override x.Matchers=[combination]             