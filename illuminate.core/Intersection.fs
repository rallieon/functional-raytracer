namespace Illuminate
open Illuminate.Types
open Illuminate.Math
open Illuminate.Coordinate

module Intersection = 
    let bias = 0.0001  //Todo, make an option?

    let calculateShadowPoint ray point normal = 
        let biasNormal = multiplyVector (normal, bias)
        addVectorToPoint point biasNormal

    let calcHitPoint origin ray tnear = 
        let evaluatedRay = {x = ray.dirX * tnear; y = ray.dirY * tnear; z = ray.dirZ * tnear}
        {x = origin.x + evaluatedRay.x; y = origin.y + evaluatedRay.y; z = origin.z + evaluatedRay.z}

    let calculateHitPointSphere (origin:WorldCoordinate, ray:Direction, tnear:float, sphere:Sphere) =
        let point = calcHitPoint origin ray tnear
        let normal = worldSubWorld point sphere.origin |> normalizeVector
        let shadowOrigin = calculateShadowPoint ray point normal
        {shape = Sphere sphere; t = tnear; point = point; normal = normal; shadowOrigin = shadowOrigin}

    let calculateHitPointPlane (origin:WorldCoordinate, ray:Direction, tnear:float, plane:Plane) =
        let point = calcHitPoint origin ray tnear
        let shadowOrigin = calculateShadowPoint ray point plane.planeNormal
        {shape = Plane plane; t = tnear; point = point; normal = plane.planeNormal; shadowOrigin = shadowOrigin}

    let intersectSphere (origin:WorldCoordinate) (ray:Direction) (sphere:Sphere) = 
        let l = worldSubWorld origin sphere.origin
        let rayAsVector = convertDirectionToVector ray
        let a = dotProduct (rayAsVector, rayAsVector)
        let b = 2. * dotProduct(rayAsVector, l)
        let c = dotProduct(l, l) - (sphere.radius * sphere.radius)
        let solQ, t0, t1 = solveQuadratic a b c

        match solQ, t0 < 0., t1 < 0. with
            | false, _, _ -> None
            | true, false, _ -> Some(calculateHitPointSphere(origin, ray, t0, sphere))
            | true, true, false -> Some(calculateHitPointSphere(origin, ray, t1, sphere))
            | true, true, true -> None

    let intersectPlane (origin:WorldCoordinate) (ray:Direction) (plane:Plane) =
        let denom = dotProduct ((ray |> convertDirectionToVector), (plane.planeNormal |> convertNormalToVector))
        let testRay = worldSubWorld plane.planePoint origin
        let t =  dotProduct(testRay, plane.planeNormal |> convertNormalToVector) / denom
        match abs denom > epsilon, t >= 0. with 
            | true, true -> Some(calculateHitPointPlane(origin, ray, t, plane))
            | _ -> None
    
    let intersectTriangle (origin:WorldCoordinate) (ray:Direction) (t:Triangle) =
        let A = worldSubWorld t.v1 t.v0
        let B = worldSubWorld t.v2 t.v0
        let N = cross A B
        let dist = dotProduct(N, N)
        let plane = {planePoint = t.v0; planeNormal = N |> convertVectorToNormal; color = t.color}

        //check and see if it hits the plane of the triangle
        let hitsPlane = intersectPlane origin ray plane
        match hitsPlane with
            | None -> None
            | Some hit ->
                //now that we have determined it hit the plane then check if the hitpoint is inside the triangle
                let edge0 = worldSubWorld t.v1 t.v0
                let pv0 = worldSubWorld hit.point t.v0
                let edge0Check = dotProduct(N, (cross edge0 pv0))

                let edge1 = worldSubWorld t.v2 t.v1
                let pv1 = worldSubWorld hit.point t.v1
                let edge1Check = dotProduct(N, (cross edge1 pv1))

                let edge2 = worldSubWorld t.v0 t.v2
                let pv2 = worldSubWorld hit.point t.v2
                let edge2Check = dotProduct(N, (cross edge2 pv2))

                match edge0Check < 0., edge1Check < 0., edge2Check < 0. with
                    | true, _, _ -> None
                    | false, true, _ -> None
                    | false, false, true -> None
                    | false, false, false -> Some({shape = Triangle t; t = hit.t; point = hit.point; normal = N; shadowOrigin = hit.shadowOrigin})

    let intersect (origin:WorldCoordinate) (ray:Direction) (shape:Shape) =
        match (shape) with
            | Sphere sphere -> intersectSphere origin ray sphere
            | Plane plane -> intersectPlane origin ray plane
            | Triangle t -> intersectTriangle origin ray t