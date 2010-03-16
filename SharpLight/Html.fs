#light
module Html
open System
open  Microsoft.FSharp.Collections
open LazyList

     
//type Html=
//         // ..just..plain..normal..text... but using &copy; and &amb;, etc.
//          HtmlString of String      
//
//          ///  <thetag {..attrs..}> ..inside.. </thetag> 
//          | HtmlTag of 
//                         // tag with an internal html
//                          String* HtmlAttr list * Html list
           

// These are the index-value pairs.
 // The empty string is a synonym for tags with no arguments.
//(not strictly HTML, but anyway).
type HtmlAttr = HtmlAttr of String * String

and Url=String
and A={href:Url}
and Tag= {htype:HtmlElementType; attributes:HtmlAttr list;children:Html list}
           
//and (Tag list -> Tag) with
                    // static member (>>>>) (v1:SomeFunc,v2:Tag list)=v1(v2)
and Html= Tag of Tag
           |HtmlString of String
          //static member (>>>>) (v1:Html,v2:Html list)=v1


and  HtmlElementType=Div 
                     |Img 
                     |A 
                     |UnsupportedTag of String

let  lift2 f  =fun b c -> fun a -> f (b a) (c a)                  
let rec to_s html=    let tagName=fun(htype)->match htype with
                                              |UnsupportedTag tag -> tag
                                              |_ -> let s =htype.ToString() in s.Substring(s.LastIndexOf('_')+1).ToLower()
                      let attributes tag= String.concat " " ( (tag.attributes |> Seq.map ( function  HtmlAttr (key ,value) -> (key + " = '" + value + "'"))))
                      let startTag tag ="<" + (tag.htype |> tagName)  + " " + attributes tag + ">"                  
                      let endTag tag="</" + (tag.htype |> tagName) + ">"
                  
                      let (+^)=lift2 (+)
                      let children tag= String.concat "" (tag.children |> Seq.map  to_s ) 
                      in match html with
                         |Tag t -> (startTag +^ children +^ endTag  ) t
                         |HtmlString s -> s
                  

let (<<) html content=match(html) with
                                |HtmlString _ ->html
                                |Tag hs  ->Tag {hs with children=hs.children @ content}
let text t= HtmlString t
let ($) (html1:Html) (htmls2:Html):Html list= list.Empty
let img src=Tag  {htype=Img;attributes=[HtmlAttr("src",src)];children=list.Empty}
let a href= Tag {htype=A;attributes=[HtmlAttr("href",href)];children=list.Empty} 
let div = Tag {htype=Div;attributes= list.Empty; children= list.Empty}

let myImage= img "www.sadekdrobi.com"
let myLink= 
         a "www.sadekdrobi.com" 
                  <<[img "image.jpg"] 
         $ HtmlString "sfsfds"



//type  'a specialF = SomeType 'a->(bool*( 'a specialF ))

//This function should be replaced by a Quotation builder to be transformed later to javascript\
//This implementation has two problems:
//1: Does not go deep in the nested Htmls
//2: The type might give the impression that it will always succeed to modify, function name is not explicit enough. Maybe should return an error in case
//This function should apply rather to Html list and return an Html list
//Computation expression might be a good candidate for constructing the Quotation

let getMatches html predicate = 
                                    let rec matcher html =  seq{match html with
                                                                    | Tag tag  when(predicate tag) -> yield tag
                                                                                                      yield! Seq.concat( tag.children|> Seq.map matcher)
                                                                    |_ ->()}
                                    in LazyList.ofSeq( matcher html)


let modifyAll (html:Html) (predicate:Tag->bool) (modifier:Tag seq ->Tag seq):Html  =  let modifiedMatches= modifier (getMatches  html predicate) 
                                                                                       in Tag ( modifiedMatches |> Seq.head) 
                                                                      

(*let modifyFirst html predicate (modify:Tag  -> Tag ) =  let onlyFirst tags=  (tags |> Seq.head |> modify) |>    Seq.cons   <| (tags|>Seq.skip 1) 
                                                        in  html |> modifyAll  <| predicate <|  onlyFirst *)

let modifiedImage=  
                    let f image= match image.htype with
                                  Img ->  {image with attributes=[image.attributes.Item 0]}
                                    |_ -> image in match myImage with
                                                             |Tag tag -> Tag (f tag)
                                                             |_ ->myImage

 //generated compiler error
//let mya = a "www.sadekdrobi.com" 
//                  >> img "fxfxfx" ++ a "www.sadekdrobi.com" 
//                                            >> [img "image.gif"]

//let component1 studentName=Img studentName
//let ggpp= match (a) with
//                      |NestedHtml  -> NestedHtml A list.Empty
//                      |img
//let  (>>>) f innerHtmls= match(f) with
//                         | :? (HtmlTag list ->HtmlTag)  -> f innerHtmls


//let tag s innerHtmls =  HtmlTag(s,list.Empty ,innerHtmls)
//let tag s  innerHtmls =  HtmlTag(s,list.Empty ,innerHtmls)
//let div = tag "Div"  
//let span s= tag "span" [HtmlString s]
//let showNameComponent name= div
//                               [span "hello"; span name]
//
//let helloJulien= showNameComponent "Julien"
