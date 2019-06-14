namespace Illuminate
open Illuminate.Types
open Illuminate.Math
open Illuminate.Coordinate
open Illuminate.Intersection
open Illuminate.Light
open Illuminate.Ray

module Render = 
    let render scene =
        let initPixels = 
            (List.init (scene.height * scene.width) 
                (fun idx -> {coordinate = (calculateScreenCoordinateFromIndex idx scene.width); color = {r = 0.; g = 0.; b = 0.}})):Image
                
        let renderedPixels = 
            initPixels
            |> List.mapi (fun idx pixel ->
                let viewPlane = {screenWidth = scene.width; screenHeight = scene.height; fov = scene.fov }
                let screenCoord = calculateScreenCoordinateFromIndex idx scene.width
                (mapScreenCoordinateToWorldCoodinate (screenCoord, viewPlane)), pixel.coordinate
            )
            |> List.map normalizeWorld
            |> List.map (fun result -> castRay result scene)
            :Image

        renderedPixels

 