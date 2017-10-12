// Learn more about F# at http://fsharp.org

open System
open Illuminate.Types
open Illuminate.Core
[<EntryPoint>]
let main argv =
    let timer = System.Diagnostics.Stopwatch()
    let width,height,fov = (640,480,30)
    let sphere = {origin = {x = 4.; y = -3.; z = -20.}; radius = 4.; color = {r = 0; g = 0; b = 255}}
    let scene = {shapes = [Sphere sphere]; lights = []; camera = {x = 0.; y = 0.; z = 0.}}
    let viewPlane = {screenHeight = height; screenWidth = width; fov=fov} 

    printfn "Rendering..."
    timer.Start()
    let t = render viewPlane scene
    createPPM t width height
    printfn "Elapsed Time: %i" timer.ElapsedMilliseconds
    0 // return an integer exit code
