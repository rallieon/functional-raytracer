namespace Illuminate
open Illuminate.Types
open Illuminate.Operators
open Illuminate.Math
open Illuminate.Coordinate
open Illuminate.Intersection
open Illuminate.Light
open Illuminate.Hit

module Render = 
    let getHitColor (hitObj:HitPoint, scene:Scene) = 
        let shape, _, _ = hitObj
        let shapeColor = 
            match shape with
                | Sphere s -> s.color
                | Plane p -> p.color

        scene.lights
            |> List.map (fun light -> hitLight(light, scene, hitObj))
            |> List.map (
                fun lightHit -> 
                    match lightHit with
                        | Some hit -> getLightIntensity hit scene
                        | None -> 0.
                )
            |> List.fold(fun color intensity -> 
                    let r = color.r + shapeColor.r * intensity
                    let g = color.g + shapeColor.g * intensity
                    let b = color.b + shapeColor.b * intensity
                    {r = r; g = g; b = b}
               ) {r = 0.; g = 0.; b = 0.}

    let castRay (ray:Direction,pixel:ScreenCoordinate) (scene:Scene) =
        let hit = getHitPoint ray scene
        let hitColor = 
            match hit with
                | Some point -> getHitColor (point, scene)
                | None -> {r = 0.; g = 0.; b = 0.}

        {coordinate = pixel; color = hitColor}

    let render scene =
        let initPixels = 
            (List.init (scene.height * scene.width) 
                (fun idx -> {coordinate = (calculateScreenCoordinateFromIndex idx scene.width); color = {r = 0.; g = 0.; b = 0.}})):Image
                
        let renderedPixels = 
            initPixels
            |> List.mapi (fun idx pixel -> (mapScreenCoordinateToWorldCoodinate ((calculateScreenCoordinateFromIndex idx scene.width), {screenWidth = scene.width; screenHeight = scene.height; fov = scene.fov })), pixel.coordinate)
            |> List.map normalizeWorld
            |> List.map (fun result -> castRay result scene)
            :Image

        renderedPixels

 