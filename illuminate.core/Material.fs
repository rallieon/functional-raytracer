namespace Illuminate
open Illuminate.Types
open Illuminate.Math
open Illuminate.Light

module Material = 
    let getHitColor hitObj scene =
        let shapeColor = 
            match hitObj.shape with
                | Sphere s -> s.color
                | Plane p -> p.color

        scene.lights
            |> List.map ( fun light -> getLightIntensity light scene hitObj )
            |> List.fold(fun color intensity -> 
                    let r = color.r + shapeColor.r * intensity
                    let g = color.g + shapeColor.g * intensity
                    let b = color.b + shapeColor.b * intensity
                    {r = r; g = g; b = b}
               ) {r = 0.; g = 0.; b = 0.}