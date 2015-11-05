module Material

open Types
open Core

// Turn a three-float RGB color into a 4-tuple ARGB value
let argbFromColor (color:Color) =
    (255,(int(color.r*255.0)), (int(color.g*255.0)), (int(color.b*255.0)))

// Gets the color from a particular light hitting a particular intersection point
let colorFromLight light intersection scene =
    let kd = intersection.material.diffuseColor
    let ks = intersection.material.specularColor
    let Is = light
    let L = norm (light.position - intersection.point)
    let V = scene.camera.position - intersection.point
    let H = norm (L + V)
    let normal = norm intersection.normal
    let HN = Vector3D.DotProduct(H,normal)
    let HN = match HN with
             |_ when HN < 0.0 -> 0.0
             |_ -> Math.Pow(HN, intersection.material.shininess)
    let LN = Vector3D.DotProduct(L,normal)
    let LN = match LN with
             |_ when LN < 0.0 -> 0.0
             |_ -> LN
    let phongFactor = ks * HN
    let diffuseFactor = kd * LN
    match shadowAt intersection light scene with
    | true -> Is.color * phongFactor
    | false -> Is.color * (diffuseFactor + phongFactor)

// calculates the color to render at a particular intersection point.
let colorAt intersection scene =
    let kd = intersection.material.diffuseColor
    let Ia = scene.lighting.ambientLight
    let ambient = kd * Ia
    let reflectedCols = List.map (fun x -> colorFromLight x intersection scene) scene.lighting.lights
    ambient + List.sum reflectedCols
