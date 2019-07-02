open Illuminate.Framework.Types
open Illuminate.Framework.Render
open Illuminate.Interface.SceneReader
open Illuminate.Interface.SceneWriter
open System.IO

[<EntryPoint>]
let main argv =
    let timer = System.Diagnostics.Stopwatch()
    let inputPath = 
        match argv.Length > 0 with
        | true -> argv.[0]
        | false -> Path.Combine("meta", "scenes", "main.json")

    let outputPath = 
        match argv.Length > 1 with
        | true -> argv.[1]
        | false -> Path.Combine("meta", "images", System.DateTime.Now.ToString("yyyy-MM-dd-HH-mm-ss") + ".jpeg")

    let scene = readScene inputPath

    printfn "Rendering..."
    timer.Start()
    let t = render scene
    printfn "Elapsed Time: %i" timer.ElapsedMilliseconds
    writeScene t scene outputPath
