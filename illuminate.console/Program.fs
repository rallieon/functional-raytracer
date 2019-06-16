open Illuminate.Types
open Illuminate.Render
open Illuminate.SceneReader
open Illuminate.SceneWriter

[<EntryPoint>]
let main argv =
    let timer = System.Diagnostics.Stopwatch()
    let inputPath = 
        match argv.Length > 0 with
        | true -> argv.[0]
        | false -> "./meta/scenes/main.json"

    let outputPath = 
        match argv.Length > 1 with
        | true -> argv.[1]
        | false -> "meta\\images\\" + System.DateTime.Now.ToString("yyyy-MM-dd HH-mm-ss") + ".jpeg"

    let scene = readScene inputPath

    printfn "Rendering..."
    timer.Start()
    let t = render scene
    printfn "Elapsed Time: %i" timer.ElapsedMilliseconds
    writeScene(t, scene, outputPath)
