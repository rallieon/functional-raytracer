open Illuminate.Types
open Illuminate.Render
open Illuminate.Image
open Illuminate.SceneReader

[<EntryPoint>]
let main argv =
    let timer = System.Diagnostics.Stopwatch()
    let scene = readScene "./scenes/test1.json"

    printfn "Rendering..."
    timer.Start()
    let t = render scene
    createPPM t scene.width scene.height
    printfn "Elapsed Time: %i" timer.ElapsedMilliseconds
    0 // return an integer exit code
