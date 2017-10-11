// Learn more about F# at http://fsharp.org

open System
open Illuminate.Types
open Illuminate.Core
[<EntryPoint>]
let main argv =
    let timer = System.Diagnostics.Stopwatch()
    let width,height = (1024,768)
    let sphere = {origin = {x = -1.; y = 1.; z = -3.}; radius = 1.; color = {r = 0; g = 0; b = 255}}
    let scene = {shapes = [Sphere sphere]; lights = []; worldWidth = 10; worldHeight = 10}
    let viewPlane = {screenHeight = height; screenWidth = width; fov=90} 

    printfn "Rendering..."
    timer.Start()
    let t = render viewPlane scene
    createPPM t width height
    printfn "Elapsed Time: %i" timer.ElapsedMilliseconds
    0 // return an integer exit code
