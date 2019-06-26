namespace Illuminate.Framework
open Illuminate.Framework.Types
open Illuminate.Framework.Math
open Illuminate.Framework.Coordinate
open Illuminate.Shapes.Box
open Illuminate.Shapes.Sphere
open Illuminate.Shapes.Plane
open Illuminate.Shapes.Triangle

module Intersection = 
    let intersect origin ray shape =
        match (shape) with
            | Sphere sphere -> intersectSphere origin ray sphere
            | Plane plane -> intersectPlane origin ray plane
            | Triangle t -> intersectTriangle origin ray t
            | Box b -> intersectBox origin ray b
            | TriangleMesh m -> None //do nothing because the triangles have already been added to scene
    
    let chooseIntersection intersection =
        match intersection with
        | Some result -> result.t
        | None -> 100000000.
            
    let trace ray origin scene =
        let inter = intersect origin ray
        scene.shapes
            |> List.map inter
            |> List.minBy chooseIntersection