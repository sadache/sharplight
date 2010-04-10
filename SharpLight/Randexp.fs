module Randexp
//#r "..//Dependencies//FSharp.PowerPack.dll"
open System
open LazyList
type Lang = LazyList<String>
let (.|.) (h:Lazy<_>) (t:Lazy<_>)= LazyList.delayed <| fun()-> LazyList.consDelayed (h.Value) <| fun () -> t.Value
let rec alt (xs:Lang) (ys:Lang) :Lang=
     match (xs,ys) with 
        LazyList.Cons(x,xt) ,LazyList.Cons(y,yt) -> match compare (String.length x, x) (String.length y, y) with
                                                         -1 -> lazy(x) .|. lazy(alt xt ys)
                                                         |0 ->lazy(x) .|. lazy(alt xt yt)
                                                         |1 -> lazy(y) .|. lazy(alt xs yt)
         |(xs, ys) -> LazyList.append xs  ys

let rec cat xs ys = match (xs,ys) with 
                     LazyList.Cons(x,xt) ,LazyList.Cons(y,yt) -> lazy (x+y) .|. lazy(alt (cat (LazyList.cons x LazyList.empty) yt) (cat xt ys))
                     | _ ,_ -> LazyList.empty


let rec clo= function LazyList.Nil -> LazyList.cons "" LazyList.empty
                      | LazyList.Cons("",xs)-> clo xs
                      |  xs -> lazy("") .|. lazy(cat xs (clo xs))



type StkSym = P of Lang   // Primary     ::= letter | "()" | "(" A ")"
             | C of Lang   // Catenation  ::= P | P "*" | C C
             | A of Lang   // Alternation ::= C | A "|" A
             | L        // "("
type Stack  = StkSym LazyList // head of list is top of stack



let isAlpha=Char.IsLetter
let rec parse (stack: Stack)  (s:char LazyList) : Lang=
        match stack,s with
         LazyList.Cons(P(x),z) ,         LazyList.Cons('*',s)            -> parse ((lazy(C(clo x))) .|. lazy(z)) s
         |LazyList.Cons(P(x),z) ,                          s             -> parse (lazy(C(x)) .|. lazy(z))    s
         |LazyList.Cons(C(y),     LazyList.Cons( C(x),z)) ,s             -> parse (lazy(C(cat x y)) .|. lazy(z)) s
         |LazyList.Cons(C(x),z) ,        LazyList.Cons('|',s)            -> parse (lazy(A(x)) .|. lazy(z)) s
         |LazyList.Cons(C(x),z) , (LazyList.Cons(')',_) as s)            -> parse (lazy(A(x)) .|. lazy(z)) s
         |LazyList.Cons(C(x),z) ,         (LazyList.Nil as s)            -> parse (lazy (A(x)) .|. lazy(z)) s
         |LazyList.Cons(A(y),    LazyList.Cons(A(x),z)),   s             -> parse (lazy(A(alt x y)) .|. lazy(z)) s
         |LazyList.Cons(A(x),LazyList.Cons(L,z)), LazyList.Cons(')',s)   -> parse (lazy(P(x)) .|. lazy(z)) s
         |LazyList.Cons(L,z),            LazyList.Cons(')',s)            -> parse (lazy(P(LazyList.cons "" empty)) .|. lazy(z)) s
         |z,                             LazyList.Cons('(',s)            -> parse (LazyList.cons L z) s
         |z,                            LazyList.Cons(c,s) when isAlpha c->  parse (LazyList.cons (P(LazyList.cons (new String([|c|])) <| LazyList.empty)) z) s
         |LazyList.Cons(A(x),LazyList.Nil), LazyList.Nil ->x
         |_, s -> raise(Exception())

let enum s = parse LazyList.empty s
let print s= printfn "%s" s
let test =LazyList.iter print <| LazyList.take 300  (enum (LazyList.ofArray  ("(b|ab*a)*".ToCharArray())))

(*
parse (P(x):z)      ('*':s) = parse (C(clo x):z)    s
parse (P(x):z)            s = parse (C(x):z)        s
parse (C(y):C(x):z)       s = parse (C(cat x y):z)  s
parse (C(x):z)      ('|':s) = parse (A(x):z)        s
parse (C(x):z)    s@(')':_) = parse (A(x):z)        s
parse (C(x):z)         s@"" = parse (A(x):z)        s
parse (A(y):A(x):z)       s = parse (A(alt x y):z)  s
parse (A(x):L:z)    (')':s) = parse (P(x):z)        s
parse (L:z)         (')':s) = parse (P[""]:z)       s

parse z             ('(':s) = parse (L:z)           s
parse z   (c:s) | isAlpha c = parse (P[[c]]:z)      s
parse [A(x)]             "" = x
parse _ s = error ("suffix where parse failed: \""++s++"\"")
*)