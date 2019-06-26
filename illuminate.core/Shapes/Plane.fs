namespace Illuminate.Shapes
open Illuminate.Framework.Types
open Illuminate.Framework.Math
open Illuminate.Framework.Coordinate
open Illuminate.Framework.Hit

module Plane = 
    let calculateHitPointPlane origin ray tnear plane =
        let point = calcHitPoint origin ray tnear
        let shadowOrigin = calculateShadowPoint ray point plane.planeNormal
        {shape = Plane plane; t = tnear; point = point; normal = plane.planeNormal; shadowOrigin = shadowOrigin}

    let intersectPlane origin ray plane =
        let denom = dot (ray |> convertDirectionToCoordinate) plane.planeNormal
        let testRay = worldSubWorld plane.planePoint origin
        let t =  dot testRay plane.planeNormal / denom
        match abs denom > epsilon, t >= 0. with 
            | true, true -> Some(calculateHitPointPlane origin ray t plane)
            | _ -> None