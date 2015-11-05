module Scene

open Types
open Core
open Shape
open Material

// recursively casts rays to determine the color a given ray should register
let rec castRay ray scene numReflections = 
    let intersects = scene.shapes 
                     |> List.collect (fun x -> intersectShape x ray)
                     |> List.filter  (fun x -> fst x > Epsilon)
    match intersects with
    | [] -> Background
    | _  -> let (time, intersection) = List.minBy (fun x -> (fst x))  intersects
            let colorAtIntersection = colorAt intersection scene
            let reflectDir = ray.direction - norm (intersection.normal * ray.direction.DotProduct(intersection.normal)) * 2.0

            let newRay = { origin = intersection.point; direction = reflectDir }
            match time with
            | _ when time > FadeDistance -> Background
            | _ -> match numReflections with
                   | _ when numReflections < MaxReflections -> 
                          ((colorAtIntersection * (1.0-intersection.material.reflectivity)) + 
                            ((castRay newRay scene (numReflections+1)) * intersection.material.reflectivity)) * 
                            ((FadeDistance-time)/FadeDistance)
                   | _ -> (colorAtIntersection * (1.0-intersection.material.reflectivity))