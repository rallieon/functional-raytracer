module Shape

open Types
open Core

// Returns the list of tuples of intersection points of the given ray with the given shape
// with the time value at which the intersection occurred.
let rec intersectShape shape ray =
    match shape with
    | Sphere (center, radius, material ) -> // intersect a sphere
        let s = ray.origin - center
        let rayDir = norm ray.direction
        let sv = Vector3D.DotProduct(s,rayDir)
        let ss = Vector3D.DotProduct(s,s)
        let discr = sv*sv - ss + radius*radius
        if discr < 0.0 then []
        else
            let normalAtTime t = norm (pointAtTime ray t - center)
            let (t1,t2) = (-sv + sqrt(discr), -sv - sqrt(discr))
            [ (t1, { normal = normalAtTime t1; point = pointAtTime ray t1; ray = ray; material = material (pointAtTime ray t1) });
              (t2, { normal = normalAtTime t2; point = pointAtTime ray t2; ray = ray; material = material (pointAtTime ray t2) }) ]
    
    | Plane  (normal, point,  material ) -> // intersect a plane
        let v = ray.direction
        let N = norm normal
        let vN = Vector3D.DotProduct(v,N)
        if vN = 0.0 then []
        else
            let t = Vector3D.DotProduct((point - ray.origin), N) / vN
            let intPoint = pointAtTime ray t
            [ (t , { normal = normal; point = pointAtTime ray t; ray = ray; material = material intPoint } ) ]

    | BoundPlane (normal, point0, point1, material) -> // intersect a bound plane
        let plane = Plane (normal, point0, material)
        let planeIntersects = intersectShape plane ray
                              |> List.filter (fun x -> fst x > Epsilon)
        let onBoundPlane intersection = // there's probably a less ugly way to do this
            intersection.point.X >= min (point0.X-Epsilon) (point1.X-Epsilon) &&
            intersection.point.Y >= min (point0.Y-Epsilon) (point1.Y-Epsilon) &&
            intersection.point.Z >= min (point0.Z-Epsilon) (point1.Z-Epsilon) &&
            intersection.point.X <= max (point0.X+Epsilon) (point1.X+Epsilon) &&
            intersection.point.Y <= max (point0.Y+Epsilon) (point1.Y+Epsilon) &&
            intersection.point.Z <= max (point0.Z+Epsilon) (point1.Z+Epsilon) 
        let result = List.filter (fun x -> onBoundPlane (snd x)) planeIntersects
        result
