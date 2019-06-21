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

    let intersect (origin:WorldCoordinate) (ray:Direction) (shape:Shape) =
        match (shape) with
            | Sphere sphere -> intersectSphere origin ray sphere
            | Plane plane -> intersectPlane origin ray plane