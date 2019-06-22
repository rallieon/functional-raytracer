namespace Illuminate
open Illuminate.Types
open Illuminate.Math
open Illuminate.Light

module Material = 
    let getHitColor hitObj scene =
        let getIntensity = getLightIntensity scene hitObj
        let shapeColor = 
            match hitObj.shape with
                | Sphere s -> s.color
                | Plane p -> p.color
                | Triangle t -> t.color
        
        let addColor color intensity =
            let r = color.r + shapeColor.r * intensity.r
            let g = color.g + shapeColor.g * intensity.g
            let b = color.b + shapeColor.b * intensity.b
            {r = r; g = g; b = b}

        scene.lights
            |> List.map getIntensity
            |> List.fold addColor {r = 0.; g = 0.; b = 0.}