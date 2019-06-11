namespace Illuminate
open Illuminate.Types
open Illuminate.Operators
open Illuminate.Math
open Illuminate.Intersection

module Hit = 
    let getHitPoint ray scene : HitPoint option =
        scene.shapes
            |> List.map (fun shape -> (intersect scene.camera ray shape))
            |> List.minBy (fun intersection -> 
                match intersection with
                    | Some result -> Some result
                    | None -> None)