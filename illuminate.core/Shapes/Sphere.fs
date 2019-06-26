namespace Illuminate.Shapes
open Illuminate.Types
open Illuminate.Math
open Illuminate.Coordinate
open Illuminate.Hit

module Sphere = 

    let calculateHitPointSphere origin ray tnear sphere =
        let point = calcHitPoint origin ray tnear
        let normal = worldSubWorld point sphere.origin |> normalizeVector
        let shadowOrigin = calculateShadowPoint ray point normal
        {shape = Sphere sphere; t = tnear; point = point; normal = normal; shadowOrigin = shadowOrigin}

    let intersectSphere origin ray sphere = 
        let l = worldSubWorld origin sphere.origin
        let v = convertDirectionToCoordinate ray
        let a = dot v v
        let b = 2. * (dot v l)
        let c = (dot l l) - (sphere.radius * sphere.radius)
        let solQ, t0, t1 = solveQuadratic a b c

        match solQ, t0 < 0., t1 < 0. with
            | false, _, _ -> None
            | true, false, _ -> Some(calculateHitPointSphere origin ray t0 sphere)
            | true, true, false -> Some(calculateHitPointSphere origin ray t1 sphere)
            | true, true, true -> None