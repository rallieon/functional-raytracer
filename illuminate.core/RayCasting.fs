namespace Illuminate
open Illuminate.Types
open Illuminate.Operators
open Illuminate.Math
open Illuminate.Coordinate

module Core = 
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
            | false, false, false, true, _, _ -> Some((Shape.Sphere sphere), t0)
            | false, false, false, false, true, _ -> Some((Shape.Sphere sphere), t1)
            | false, false, false, false, false, true -> Some((Shape.Sphere sphere), if t1 < t0 then t1 else t0)
            | false, false, false, false, false, false -> None
    
    let intersectPlane (camera:Camera) (ray:Direction) (plane:Plane) =
        Some((Shape.Plane plane), 0.)

    let intersect (camera:Camera) (ray:Direction) (shape:Shape) =
        match (shape) with
            | Sphere sphere -> intersectSphere camera ray sphere
            | Plane plane -> intersectPlane camera ray plane

    let getHitPoint ray scene =
        scene.shapes
            |> List.map (fun shape -> (intersect scene.camera ray shape))
            |> List.minBy (fun intersection -> 
                match intersection with
                    | Some result -> snd(result)
                    | None -> 100000000000.)

    let getHitColor (hitObj:Shape * float) = 
        match (fst hitObj) with
            | Sphere sphere -> sphere.color
            | Plane plane -> plane.color

    let castRay (ray:Direction,pixel:ScreenCoordinate) (scene:Scene) =
        let hit = getHitPoint ray scene
        
        let hitColor = 
            match hit with
                | Some point -> getHitColor point
                | None -> {r = 0; g = 0; b = 0}

        {coordinate = pixel; color = hitColor}

    let render scene =
        let initPixels = 
            (List.init (scene.height * scene.width) 
                (fun idx -> {coordinate = (calculateScreenCoordinateFromIndex idx scene.width); color = {r = 0; g = 0; b = 0}})):Image
                
        let renderedPixels = 
            initPixels
            |> List.mapi (fun idx pixel -> (mapScreenCoordinateToWorldCoodinate ((calculateScreenCoordinateFromIndex idx scene.width), {screenWidth = scene.width; screenHeight = scene.height; fov = scene.fov })), pixel.coordinate)
            |> List.map normalizeWorld
            |> List.map (fun result -> castRay result scene)
            :Image

        renderedPixels

 