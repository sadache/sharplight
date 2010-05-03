module SharpLight.Core
open FunUtils
open System
open Microsoft.FSharp.Collections
open LazyList
open Html
open System.Web
open Reader
open Maybe

type Url= String
type  Request= {Request:Web.HttpRequest;UrlParts:string ;Method:Method;AcceptedMime: string seq;UrlParams:Map<string,string>}
and   Method= Get|Post|Put|Unsupported
type ResponseMeta={ Status:int*string;MimeType:string}
type 'a Response =ResponseMeta * 'a
type 'a Servlet=  Reader<Request,'a  Response>
type Matcher = abstract member Do: (Request->  byte seq Servlet Option)
               abstract member Info:String

let matchIt rq (matchers:Matcher list)=  [for m in matchers -> m.Do]|> Seq.tryPick ((|>) rq) 

[<AbstractClass>]
type LightController() = 
           abstract member Matchers : Matcher list
           interface  System.Web.IHttpHandler with
                member x.ProcessRequest context= 
                        let makeRequest r=   {Request=r; 
                                              UrlParams=Map.empty;
                                              UrlParts= r.Path
                                              Method=match r.HttpMethod with |"GET"->Get |"Post" ->Post|_-> Unsupported;
                                              AcceptedMime=r.AcceptTypes}
                        let request=makeRequest context.Request 
                        let   matched=matchIt request x.Matchers                                                          
                        in match matched with
                               |None-> context.Response.StatusCode <- 404
                               |Some servlet -> Seq.iter ( fun c -> context.Response.OutputStream.WriteByte (c:byte)) <| snd (runReader servlet request)
                member x.IsReusable= true


//TODO I might need a State monad here!
//matchers

let dirWith (matchingFunction) (dir:string) (subs :Matcher  list) : Matcher=  
       let matchingFunction:string->(Map<string,string> * string ) Option=matchingFunction dir
       in {new Matcher with
            member x.Do=fun rq-> 
                Option.bind <| (fun r-> matchIt <| {rq with UrlParts= snd r;UrlParams= UrlPatterns.concatMap rq.UrlParams <| fst r }
                                                <| subs)
                            <|(matchingFunction rq.UrlParts)

            member x.Info= "not documented"  }

let dirX = dirWith UrlPatterns.matchXUrl
let dir_ = dirWith UrlPatterns.matchUrl_
let dir  = dirWith UrlPatterns.matchUrlSectionedNoGreedy

//let preFantom (matcher:Request -> Reader<Request,byte seq  Response Endo> Option ) (subs :Matcher  list) :Matcher= 
        

let postFantom (matcher:Request -> Reader<Request,byte seq  Response Endo> Option ) (subs :Matcher  list) :Matcher= 
        {new Matcher with member x.Do= fun req -> maybe{let! endo=matcher req
                                                        let! xx= matchIt req subs
                                                        return endo <*> xx}
                          member x.Info= "Any"}   

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

let web_get  s= webMethod Get s
let web_post s= webMethod Post s



let webMethod1 m (subs :Matcher  list) =
    {new Matcher with
        member x.Do = fun(rq) -> if rq.Method = m then matchIt rq subs else None
        member x.Info= m.ToString()}

//composable partial responses
//not sure how doable it is in F# but a better type could be a ReaderT<Request,State<MetaResponse,'a>>
type ComposableServlet<'a,'b>= Reader<Request,'a Response -> 'b Response>


let yield_string (s:string):ComposableServlet<_,Char seq>=  reader{return fun (meta,_) -> (meta,Seq.ofArray <| s.ToCharArray())}                                                                    
                                                         
let mime t :ComposableServlet<'a,'a> = reader{return fun (meta,content) -> {meta with MimeType=t},content}
let code code :ComposableServlet<'a,'a>=reader{return fun (meta,content) ->{meta with Status=(code,"")},content}
let ok<'a> :ComposableServlet<'a,'a>  = code 200 
let text_html= "text/html"
let no_content (s:string)=code 204  >>> mime text_html >>> yield_string  s


let yield_html (html:Html)= mime text_html >>> yield_string (html |> to_s) 
                                                       
//let yield_stream  (s:IO.Stream):Servlet= fun res  _ -> {res with  Content= s.

//using data in Request object
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