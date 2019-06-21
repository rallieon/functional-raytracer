namespace Illuminate
open Illuminate.Types
open Illuminate.Intersection

module Hit = 
    let chooseIntersection intersection =
        match intersection with
        | Some result -> result.t
        | None -> 100000000.
            
    let getHitPoint ray origin scene =
        let inter = intersect origin ray
        scene.shapes
            |> List.map inter
            |> List.minBy chooseIntersection