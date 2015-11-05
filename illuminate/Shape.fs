module Shape

open Types
open Core
open Ray

// Returns the list of tuples of intersection points of the given ray with the given shape
// with the time value at which the intersection occurred.
let rec intersectShape shape ray =
    match shape with
    | Sphere (center, radius, material ) -> // intersect a sphere
        let s = ray.origin - center
        let rayDir = norm ray.direction
        let sv = s.DotProduct(rayDir)
        let ss = s.DotProduct(s)
        let discr = sv*sv - ss + radius*radius
        if discr < 0.0 then []
        else
            let normalAtTime t = norm (pointAtTime (ray, t) - center)
            let (t1,t2) = (-sv + sqrt(discr), -sv - sqrt(discr))
            [ (t1, { normal = normalAtTime t1; point = pointAtTime (ray, t1); ray = ray; material = material (pointAtTime (ray, t1)) });
              (t2, { normal = normalAtTime t2; point = pointAtTime (ray, t2); ray = ray; material = material (pointAtTime (ray, t2)) }) ]
    
    | Plane  (normal, point,  material ) -> // intersect a plane
        let v = ray.direction
        let N = norm normal
        let vN = v.DotProduct(N)
        if vN = 0.0 then []
        else
            let t = (point - ray.origin).DotProduct(N) / vN
            let intPoint = pointAtTime (ray, t)
            [ (t , { normal = normal; point = pointAtTime (ray, t); ray = ray; material = material intPoint } ) ]

    | BoundPlane (normal, point0, point1, material) -> // intersect a bound plane
        let plane = Plane (normal, point0, material)
        let planeIntersects = intersectShape plane ray
                              |> List.filter (fun x -> fst x > Epsilon)
        let onBoundPlane intersection = // there's probably a less ugly way to do this
            intersection.point.x >= min (point0.x-Epsilon) (point1.x-Epsilon) &&
            intersection.point.y >= min (point0.y-Epsilon) (point1.y-Epsilon) &&
            intersection.point.z >= min (point0.z-Epsilon) (point1.z-Epsilon) &&
            intersection.point.x <= max (point0.x+Epsilon) (point1.x+Epsilon) &&
            intersection.point.y <= max (point0.y+Epsilon) (point1.y+Epsilon) &&
            intersection.point.z <= max (point0.z+Epsilon) (point1.z+Epsilon) 
        let result = List.filter (fun x -> onBoundPlane (snd x)) planeIntersects
        result
