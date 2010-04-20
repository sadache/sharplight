module Reader
type Reader<'r,'a> = Reader of ('r -> 'a)
let runReader (Reader f) env = f env
let returnR :'a ->Reader<'r,'a> = fun a -> Reader (fun _ -> a)
let bind : Reader<'r,'a> -> ('a -> Reader<'r, 'b>) -> Reader<'r, 'b>= 
    fun m k -> Reader (fun r -> runReader (k (runReader m r)) r)
type ReaderBuilder() =
  member this.Return(a)  = 
    Reader (fun _ -> a)

  member this.Bind(m, k) = 
    Reader (fun r -> runReader (k (runReader m r)) r)
  member this.ReturnFrom(a)=a
  member this.Combine a b= a >> b
let reader = ReaderBuilder()
let ask = Reader (id)

let asks f = reader {
  let! r = ask
  return (f r) }

let (>>>): Reader<'r,('a->'b)> -> Reader<'r,('b->'c)> -> Reader<'r,('a->'c)>=fun f_ g_ -> reader{let! f= f_
                                                                                                 let! g= g_
                                                                                                 return f >> g}
let (<*>):Reader<'r,('a->'b)> -> Reader<'r,'a>-> Reader<'r,('b)>= fun f_ b_ -> reader{let! f=f_
                                                                                      let! b=b_
                                                                                      return f b}
