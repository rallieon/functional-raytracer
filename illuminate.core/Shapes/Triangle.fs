namespace Illuminate.Shapes
open Illuminate.Types
open Illuminate.Math
open Illuminate.Coordinate
open Illuminate.Shapes.Plane
open Illuminate.Hit

module Triangle = 
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