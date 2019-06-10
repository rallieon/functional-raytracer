namespace Illuminate
open Illuminate.Types
open Illuminate.Operators
open Illuminate.Math
open Illuminate.Coordinate
open Illuminate.Intersection

module Core = 
    let getHitPoint ray scene =
        scene.shapes
            |> List.map (fun shape -> (intersect scene.camera ray shape))
            |> List.minBy (fun intersection -> 
                match intersection with
                    | Some result -> snd(result)
                    | None -> 100000000000.)

    let getHitColor (hitObj:Shape * float) = 
        match (fst hitObj) with
            | Sphere sphere -> sphere.color
            | Plane plane -> plane.color

    let castRay (ray:Direction,pixel:ScreenCoordinate) (scene:Scene) =
        let hit = getHitPoint ray scene
        
        let hitColor = 
            match hit with
                | Some point -> getHitColor point
                | None -> {r = 0; g = 0; b = 0}

        {coordinate = pixel; color = hitColor}

    let render scene =
        let initPixels = 
            (List.init (scene.height * scene.width) 
                (fun idx -> {coordinate = (calculateScreenCoordinateFromIndex idx scene.width); color = {r = 0; g = 0; b = 0}})):Image
                
        let renderedPixels = 
            initPixels
            |> List.mapi (fun idx pixel -> (mapScreenCoordinateToWorldCoodinate ((calculateScreenCoordinateFromIndex idx scene.width), {screenWidth = scene.width; screenHeight = scene.height; fov = scene.fov })), pixel.coordinate)
            |> List.map normalizeWorld
            |> List.map (fun result -> castRay result scene)
            :Image

        renderedPixels

 