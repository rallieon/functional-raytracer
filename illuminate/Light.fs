module Light

open Types
open Core
open Shape

// Figures out if a particular point is in shadow from a particular light or not
let shadowAt (intersection:Intersection, light:Light, scene:Scene) =
    let lightDir = norm (light.position - intersection.point)
    let ray = { origin = intersection.point; direction = lightDir }
    let lightDirs = List.map (fun (x:Light) -> (norm (x.position - intersection.point))) scene.lighting.lights
    let list = scene.shapes
               |> List.collect (fun x -> intersectShape x ray)
               |> List.filter (fun x -> (fst x) > Epsilon)
    not list.IsEmpty