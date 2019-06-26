namespace Illuminate.Shapes
open Illuminate.Framework.Types
open Illuminate.Framework.Math
open Illuminate.Framework.Coordinate
open Illuminate.Shapes.Plane
open Illuminate.Framework.Hit

module Triangle = 
    let getTriangleNormal t =
        match t.triangleNormal with
            | Some n -> n
            | None ->
                let A = worldSubWorld t.v1 t.v0
                let B = worldSubWorld t.v2 t.v0
                cross A B
    
    let isInTriangle N point t =
        let edge0 = worldSubWorld t.v1 t.v0
        let pv0 = worldSubWorld point t.v0
        let edge0Check = dot N (cross edge0 pv0)

        let edge1 = worldSubWorld t.v2 t.v1
        let pv1 = worldSubWorld point t.v1
        let edge1Check = dot N (cross edge1 pv1)

        let edge2 = worldSubWorld t.v0 t.v2
        let pv2 = worldSubWorld point t.v2
        let edge2Check = dot N (cross edge2 pv2)
        (edge0Check < 0., edge1Check < 0., edge2Check < 0.)
        
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