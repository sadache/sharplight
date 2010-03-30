module SharpLight.Core

open System
open  Microsoft.FSharp.Collections
open LazyList
open Html
open System.Web

let swap f = fun b  a-> f a b

type Servlet=  Request->Response
and Url= String
and Request= {Request:Web.HttpRequest;UrlParts:string LazyList;Method:Method;AcceptedMime: string seq}
and Method= Get|Post|Put|Unsupported
and Response= Web.HttpResponse -> unit
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
                                               |Some servlet -> servlet request context.Response
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

let lift2 f  =fun b c -> fun a -> f (b a) (c a)   
let lift1 f= fun g a -> f(g(a))
let lift0 a= fun(_) -> a                                                   
let(+^)= lift2 (fun u1 u2 -> do u1
                             u2)
let(+^^)= lift2 (+^)



let (-|) s1 s2:Servlet= s1 +^^ s2

let (|=)= (<|)
let (=>)= (<|)

let yield_string (s:string):Servlet= fun _ -> fun r -> do r.Output.Write s 
                                                          r.Flush()
                                                         
let mime t :Servlet = fun _ -> fun r -> r.ContentType <- t
let code code :Servlet=fun _ -> fun r -> r.StatusCode  <- code
let ok:Servlet = code 200 
let text_html= "text/html"
let no_content (s:string)=code 204  -| mime text_html -| yield_string  s


let yield_html (html:Html)= mime text_html -| yield_string (html |> to_s) 
                                                       
let yield_stream  (s:IO.Stream):Servlet= fun _ ->fun (r:HttpResponse) -> do r.Output.Write s 
                                                                         r.Flush() 


let from_data f ts : Servlet= fun rq-> f(rq) |> ts <| rq
                                         
let doNothing=fun(rq)->fun(r)->()



let combination= dir "products" [dir "old"
                               [webMethod Get doNothing;
                                webMethod Post ( ok -| yield_string "post on products/old")]]
                               



                    
                  
type SimpleDispatcher= inherit LightController override x.Matchers=[combination]             