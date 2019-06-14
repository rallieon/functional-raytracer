namespace Illuminate
open Illuminate.Types
open Illuminate.Math
open Illuminate.Intersection

module Hit = 
    let getHitPoint ray origin scene =
        scene.shapes
            |> List.map (fun shape -> (intersect origin ray shape))
            |> List.minBy (fun intersection -> 
                match intersection with
                    | Some result -> Some result.t
                    | None -> None)