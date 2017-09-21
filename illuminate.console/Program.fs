// Learn more about F# at http://fsharp.org

open System
open Illuminate
[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    let x = add 1 2
    printfn "Hello, the addition is %d" x
    0 // return an integer exit code
