module Randexp
open System
type Lang = LazyList<String>
val alt  : Lang -> Lang -> Lang // alternation (merge languages)
val cat  : Lang -> Lang -> Lang // catenation (product of languages)
val clo  : Lang -> Lang         // Kleene closure
val enum : char LazyList -> Lang
