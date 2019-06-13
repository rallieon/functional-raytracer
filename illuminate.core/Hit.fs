namespace Illuminate
open Illuminate.Types
open Illuminate.Operators
open Illuminate.Math
open Illuminate.Intersection

module Hit = 
    let getHitPoint ray origin scene : HitPoint option =
        scene.shapes
            |> List.map (fun shape -> (intersect origin ray shape))
            |> List.minBy (fun intersection -> 
                match intersection with
                    | Some result -> Some result
                    | None -> None)