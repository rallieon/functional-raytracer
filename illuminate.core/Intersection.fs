namespace Illuminate
open Illuminate.Types
open Illuminate.Math
open Illuminate.Coordinate

module Intersection = 
    let calculateShadowPoint ray point normal = 
        let biasNormal = multiplyVector normal bias
        addVectorToPoint point biasNormal

    let calcHitPoint origin ray tnear = 
        let evaluatedRay = {x = ray.dirX * tnear; y = ray.dirY * tnear; z = ray.dirZ * tnear}
        {x = origin.x + evaluatedRay.x; y = origin.y + evaluatedRay.y; z = origin.z + evaluatedRay.z}

    let calculateHitPointSphere origin ray tnear sphere =
        let point = calcHitPoint origin ray tnear
        let normal = worldSubWorld point sphere.origin |> normalizeVector
        let shadowOrigin = calculateShadowPoint ray point normal
        {shape = Sphere sphere; t = tnear; point = point; normal = normal; shadowOrigin = shadowOrigin}

    let calculateHitPointPlane origin ray tnear plane =
        let point = calcHitPoint origin ray tnear
        let shadowOrigin = calculateShadowPoint ray point plane.planeNormal
        {shape = Plane plane; t = tnear; point = point; normal = plane.planeNormal; shadowOrigin = shadowOrigin}

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

    let intersectPlane origin ray plane =
        let denom = dot (ray |> convertDirectionToCoordinate) plane.planeNormal
        let testRay = worldSubWorld plane.planePoint origin
        let t =  dot testRay plane.planeNormal / denom
        match abs denom > epsilon, t >= 0. with 
            | true, true -> Some(calculateHitPointPlane origin ray t plane)
            | _ -> None
    
    let intersectTriangle origin ray t =
        let N = getTriangleNormal t
        let plane = {planePoint = t.v0; planeNormal = N; color = t.color}

        //check and see if it hits the plane of the triangle
        let hitsPlane = intersectPlane origin ray plane
        match hitsPlane with
            | None -> None
            | Some hit ->
                //now that we have determined it hit the plane then check if the hitpoint is inside the triangle
                match isInTriangle N hit.point t with
                    | true, _, _ -> None
                    | false, true, _ -> None
                    | false, false, true -> None
                    | false, false, false -> Some({shape = Triangle t; t = hit.t; point = hit.point; normal = N; shadowOrigin = hit.shadowOrigin})

    let intersect origin ray shape =
        match (shape) with
            | Sphere sphere -> intersectSphere origin ray sphere
            | Plane plane -> intersectPlane origin ray plane
            | Triangle t -> intersectTriangle origin ray t