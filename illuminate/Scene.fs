module Scene

open Types
open Core
open Shape
open Material
open System.Drawing

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

let buildScene scene =
    // Make ourselves a canvas
    let width = scene.width
    let height = scene.height

    // Vertical and horizontal field of view
    let hfov = System.Math.PI/3.2
    let vfov = hfov * float(height)/float(width)

    // Pixel width and height
    let pw = 2.0 * System.Math.Tan(float(hfov/2.0))/float(width)
    let ph = 2.0 * System.Math.Tan(float(vfov/2.0))/float(height)

    // set up the coordinate system
    let n = norm (scene.camera.position - scene.camera.lookAt)
    let u = norm (scene.camera.lookUp.CrossProduct(n))
    let v = norm (n.CrossProduct(u))
    let vpc = scene.camera.position - n
    
    let image = Array.Parallel.init width (fun x ->
                Array.init height (fun y -> 
                    let rayPoint = vpc + u*float(x-width/2)*pw + v*float(y-height/2)*ph
                    let rayDir = norm (rayPoint - scene.camera.position)
                    let ray = { origin = scene.camera.position; direction = rayDir }
                    let color = castRay ray scene 0
                    let (a,r,g,b) = argbFromColor(color)
                    Color.FromArgb(a,r,g,b)))
    image