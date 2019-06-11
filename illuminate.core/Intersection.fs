namespace Illuminate
open Illuminate.Types
open Illuminate.Operators
open Illuminate.Math
open Illuminate.Coordinate

module Intersection = 
    let calculateHitPoint (camera:Camera, ray:Direction, tnear:float) =
        let evaluatedRay = {x = ray.dirX * tnear; y = ray.dirY * tnear; z = ray.dirZ * tnear}
        {x = camera.x + evaluatedRay.x; y = camera.y + evaluatedRay.y; z = camera.z + evaluatedRay.z}

    let intersectSphere (camera:Camera) (ray:Direction) (sphere:Sphere) = 
        let l = worldSubWorld sphere.origin camera
        let tca = dotProduct (l, (ray.dirX, ray.dirY, ray.dirZ))
        let d2 = (dotProduct (l, l)) - (tca * tca)
        let thc = sqrt (sphere.radius - d2)
        let t0 = tca - thc;
        let t1 = tca + thc;

        //TODO Comment
        match tca < 0., d2 > sphere.radius, t1 < 0. && t0 < 0., t1 < 0. && t0 >= 0., t0 < 0. && t1 >=0., t0 >= 0. && t1 >= 0. with
            | true, _, _, _, _, _ -> None
            | false, true, _, _, _, _ -> None
            | false, false, true, _, _, _ -> None
            | false, false, false, true, _, _ -> Some((Shape.Sphere sphere), t0, calculateHitPoint(camera, ray, t0) )
            | false, false, false, false, true, _ -> Some((Shape.Sphere sphere), t1, calculateHitPoint(camera, ray, t1))
            | false, false, false, false, false, true -> Some((Shape.Sphere sphere), (if t1 < t0 then t1 else t0), calculateHitPoint(camera, ray, (if t1 < t0 then t1 else t0)))
            | false, false, false, false, false, false -> None
    
    let intersectPlane (camera:Camera) (ray:Direction) (plane:Plane) =
        Some((Shape.Plane plane), 0., calculateHitPoint(camera, ray, 0.))

    let intersect (camera:Camera) (ray:Direction) (shape:Shape) =
        match (shape) with
            | Sphere sphere -> intersectSphere camera ray sphere
            | Plane plane -> intersectPlane camera ray plane