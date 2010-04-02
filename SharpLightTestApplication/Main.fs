#light
namespace DefaultDispatcher
open SharpLight.Core
open Html
open Reader

module Sample  =                                                                             
 let repeat times a = Seq.toList <| Seq.unfold (fun times -> if times>0 then Some (a , times-1) else None) times
 let myComponent= div << repeat 10   
                      (div << [a  "http://www.sadekdrobi.com" << [text "sadek"];
                               img "http://sadekdrobi.com/wp-content/uploads/2009/03/image2.png";
                               text "Mosquito Programming vs Functional Programming"])
                               

 let (./.)=(<|)
                                   
type SimpleDispatcher()= inherit LightController() 
                             override x.Matchers=[dir "sharplight" [dir "friends" [dir "sadek.text" [web_post <|( ok -| (yield_string "no post for sadek") <*> blank)];
                                                                                                                        
                                                                                   dir "sadek.htm"  [web_get <| let html =a "http://www.sadekdrobi.com" 
                                                                                                                                    << [text "sadek"]
                                                                                                                       in ok 
                                                                                                                           -| (yield_html  Sample.myComponent) <*> blank ]]];
                                                                                                                               
                                                                        
                                                   any <| (ok 
                                                           -| (mime text_html)
                                                           -| (from_data (fun rq -> rq.Request.Url.PathAndQuery) yield_string) <*> blank) ]
                                                           
                                                           



