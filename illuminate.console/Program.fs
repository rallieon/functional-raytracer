// Learn more about F# at http://fsharp.org

open System
open Illuminate.Core
[<EntryPoint>]
let main argv =
    let timer = System.Diagnostics.Stopwatch()
    let width,height = (1024,768)
    
    printfn "Rendering..."
    timer.Start()
    let t = render {screenHeight = height; screenWidth = width; fov=90}
    createPPM t width height
    printfn "Elapsed Time: %i" timer.ElapsedMilliseconds
    0 // return an integer exit code
