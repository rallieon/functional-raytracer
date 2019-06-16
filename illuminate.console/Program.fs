open Illuminate.Types
open Illuminate.Render
open Illuminate.Image
open Illuminate.SceneReader
open Illuminate.SceneWriter

[<EntryPoint>]
let main argv =
    let timer = System.Diagnostics.Stopwatch()
    let scene = readScene "./meta/scenes/test1.json"

    printfn "Rendering..."
    timer.Start()
    let t = render scene
    printfn "Elapsed Time: %i" timer.ElapsedMilliseconds
    writeScene(t, scene)
