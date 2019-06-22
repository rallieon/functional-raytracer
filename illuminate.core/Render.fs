namespace Illuminate
open Illuminate.Types
open Illuminate.Math
open Illuminate.Coordinate
open Illuminate.Intersection
open Illuminate.Light
open Illuminate.Ray

module Render = 
    let initPixels scene = 
        (List.init (scene.height * scene.width) 
            (fun idx -> 
                {
                    coordinate = (calculateScreenCoordinateFromIndex idx scene.width); 
                    pixelColor = {r = 0.; g = 0.; b = 0.}
                })):Image    

    let render scene =
        let coordinateBuilder = buildCoordinate scene
        let initPixels = initPixels scene
        let renderRay = castRay scene

        let renderedPixels = 
            initPixels
            |> List.mapi coordinateBuilder
            |> List.map normalizeWorld
            |> List.map renderRay
            :Image

        renderedPixels

 