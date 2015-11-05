module Light

open Types
open Core

// Figures out if a particular point is in shadow from a particular light or not
let shadowAt intersection light scene =
    let lightDir = norm (light.position - intersection.point)
    let ray = { origin = intersection.point; direction = lightDir }
    let lightDirs = List.map (fun x -> (norm (x.position - intersection.point))) scene.lighting.lights
    let list = scene.shapes
               |> List.collect (fun x -> intersectShape x ray)
               |> List.filter (fun x -> (fst x) > Epsilon)
    not list.IsEmpty