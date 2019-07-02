namespace Illuminate.Shapes
open Illuminate.Framework.Types
open Illuminate.Framework.Math
open Illuminate.Framework.Coordinate
open Illuminate.Framework.Hit
open FsAlg.Generic

module Sphere = 

    let calculateHitPointSphere origin ray tnear sphere =
        let point = calcHitPoint(origin, ray, tnear)
        let normal = point - sphere.origin |> Vector.unitVector
        let shadowOrigin = calculateShadowPoint(ray, point, normal)
        {shape = Sphere sphere; t = tnear; point = point; normal = normal; shadowOrigin = shadowOrigin}

    let intersectSphere origin ray sphere = 
        let l = origin - sphere.origin
        let a = ray * ray
        let b = (ray * l) * 2.
        let c = (l * l) - (sphere.radius * sphere.radius)
        let solQ, t0, t1 = solveQuadratic a b c

        match solQ, t0 < 0., t1 < 0. with
            | false, _, _ -> None
            | true, false, _ -> Some(calculateHitPointSphere origin ray t0 sphere)
            | true, true, false -> Some(calculateHitPointSphere origin ray t1 sphere)
            | true, true, true -> None