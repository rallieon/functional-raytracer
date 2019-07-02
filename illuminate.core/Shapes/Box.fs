namespace Illuminate.Shapes
open Illuminate.Framework.Types
open Illuminate.Framework.Math
open Illuminate.Framework.Coordinate
open Illuminate.Framework.Hit
open FsAlg.Generic

module Box = 
    let getBoxTValues (origin:WorldCoordinate, ray:Direction, b:Box) =
        let posXMin = (b.vMin.[0] - origin.[0]) / ray.[0]
        let posXMax = (b.vMax.[0] - origin.[0]) / ray.[0]
        let posYMin = (b.vMin.[1] - origin.[1]) / ray.[1]
        let posYMax = (b.vMax.[1] - origin.[1]) / ray.[1]
        let posZMin = (b.vMin.[2] - origin.[2]) / ray.[2]
        let posZMax = (b.vMax.[2] - origin.[2]) / ray.[2]

        ( 
        (if posXMin > posXMax then posXMax else posXMin),
        (if posXMin < posXMax then posXMax else posXMin),
        (if posYMin > posYMax then posYMax else posYMin),
        (if posYMin < posYMax then posYMax else posYMin),
        (if posZMin > posZMax then posZMax else posZMin),
        (if posZMin < posZMax then posZMax else posZMin)
        )

    let getBoxNormal(b:Box, h:WorldCoordinate) =
        let centerPoint = vector [(b.vMax.[0] - b.vMin.[0]) / 2.; (b.vMax.[1] - b.vMin.[1]) / 2.; (b.vMax.[2] - b.vMin.[2]) / 2.;]
        let diff = h - centerPoint
        let diffX = abs diff.[0]
        let diffY = abs diff.[1]
        let diffZ = abs diff.[2]

        //which ever side the hitpoint is closest to will be the normal needed.
        //this should for AABB, but will likely need to change when transformations are included
        match diffX > diffY && diffX > diffZ, diffY > diffX && diffY > diffZ, diffZ > diffX && diffZ > diffY with
            | true, _, _ -> vector [1.; 0.; 0.]
            | false, true, _ -> vector [0.; 1.; 0.]
            | false, false, true -> vector [0.; 0.; 1.]
            | _-> vector [0.; 0.; 1.]


    let intersectBox origin ray b = 
        let txMin,txMax,tyMin,tyMax,tzMin,tzMax = getBoxTValues(origin, ray, b)

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
                        let h = (calcHitPoint(origin, ray, tMin))
                        let N = getBoxNormal(b, h)
                        let shadowPoint = calculateShadowPoint(ray, h, N)
                        Some({shape = Box b; t = tMin; point = h; normal = N; shadowOrigin = shadowPoint})