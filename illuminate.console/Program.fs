open Illuminate.Types
open Illuminate.Core
open System.IO
open FSharp.Json

[<EntryPoint>]
let main argv =
    let timer = System.Diagnostics.Stopwatch()
    let scene = Json.deserialize<Scene>(File.ReadAllText("./scenes/test1.json"))

    printfn "Rendering..."
    timer.Start()
    let t = render scene
    createPPM t scene.width scene.height
    printfn "Elapsed Time: %i" timer.ElapsedMilliseconds
    0 // return an integer exit code
