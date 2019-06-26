namespace Illuminate.Shapes
open Illuminate.Types
open Illuminate.Math
open Illuminate.Coordinate
open Illuminate.Hit

module Box = 
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