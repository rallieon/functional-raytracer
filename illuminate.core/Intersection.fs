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


    let getBoxTValues origin ray b =
        let posXMin = (b.vMin.x - origin.x) / ray.dirX
        let posXMax = (b.vMax.x - origin.x) / ray.dirX
        let posYMin = (b.vMin.y - origin.y) / ray.dirY
        let posYMax = (b.vMax.y - origin.y) / ray.dirY
        let posZMin = (b.vMin.z - origin.z) / ray.dirZ
        let posZMax = (b.vMax.z - origin.z) / ray.dirZ

        ( 
        (if posXMin > posXMax then posXMax else posXMin),
        (if posXMin < posXMax then posXMax else posXMin),
        (if posYMin > posYMax then posYMax else posYMin),
        (if posYMin < posYMax then posYMax else posYMin),
        (if posZMin > posZMax then posZMax else posZMin),
        (if posZMin < posZMax then posZMax else posZMin)
        )

    let getBoxNormal b h =
        let centerPoint = {x = (b.vMax.x - b.vMin.x) / 2.; y = (b.vMax.y - b.vMin.y) / 2.; z = (b.vMax.z - b.vMin.z) / 2.; }
        let diff = worldSubWorld h centerPoint
        let diffX = abs diff.x
        let diffY = abs diff.y
        let diffZ = abs diff.z

        //which ever side the hitpoint is closest to will be the normal needed.
        //this should for AABB, but will likely need to change when transformations are included
        match diffX > diffY && diffX > diffZ, diffY > diffX && diffY > diffZ, diffZ > diffX && diffZ > diffY with
            | true, _, _ -> {x = 1.; y = 0.; z = 0.}
            | false, true, _ -> {x = 0.; y = 1.; z = 0.}
            | false, false, true -> {x = 0.; y = 0.; z = 1.}
            | _-> {x = 0.; y = 0.; z = 1.}


    let intersectBox origin ray b = 
        let txMin,txMax,tyMin,tyMax,tzMin,tzMax = getBoxTValues origin ray b

        match txMin > tyMax, tyMin > txMax with
            | true, _ -> None
            | _, true -> None
            | false, false -> 
                let tSecondMin = if tyMin > txMin then tyMin else txMin
                let tSecondMax = if tyMax < txMax then tyMax else txMax

                match tSecondMin > tzMax, tzMin > tSecondMax with
                    | true, _ -> None
                    | _, true -> None
                    | false, false ->
                        let tMin = if tzMin > tSecondMin then tzMin else tSecondMin
                        let tMax = if tzMax < tSecondMax then tzMax else tSecondMax
                        let h = (calcHitPoint origin ray tMin)
                        let N = getBoxNormal b h
                        let shadowPoint = calculateShadowPoint ray h N
                        Some({shape = Box b; t = tMin; point = h; normal = N; shadowOrigin = shadowPoint})

    let intersect origin ray shape =
        match (shape) with
            | Sphere sphere -> intersectSphere origin ray sphere
            | Plane plane -> intersectPlane origin ray plane
            | Triangle t -> intersectTriangle origin ray t
            | Box b -> intersectBox origin ray b
            | TriangleMesh m -> None //do nothing because the triangles have already been added to scene