namespace Illuminate.Framework
open Illuminate.Framework.Types
open Illuminate.Framework.Math
open Illuminate.Framework.Coordinate
open Illuminate.Framework.Intersection
open Illuminate.Lighting.Light
open Illuminate.Framework.Ray

module Render = 
    let initPixels scene = 
        let convertIdx idx =
            { coordinate = (calculateScreenCoordinateFromIndex idx scene.width); pixelColor = {r = 0.; g = 0.; b = 0.} }

        List.init (scene.height * scene.width) convertIdx

    let render scene =
        let coordinateBuilder = buildCoordinate scene
        let initPixels = initPixels scene
        let renderRay = castRay scene

        let renderedPixels = 
            initPixels
            |> List.mapi coordinateBuilder
            |> List.map normalizeWorld
            |> List.map renderRay

        renderedPixels