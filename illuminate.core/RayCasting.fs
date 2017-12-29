namespace Illuminate
open Illuminate.Types
open Illuminate.Operators

module Core = 

    let createPPM (image:Image) width height = 
        let wr = new System.IO.StreamWriter("test.ppm")
        wr.WriteLine "P3"
        wr.WriteLine("{0} {1}", width, height)  //use writeline instead of printf for performance
        wr.WriteLine "255"
        ignore (image |> List.map(fun pixel -> (wr.Write("{0} {1} {2} ", pixel.color.r, pixel.color.g, pixel.color.b))))
        wr.Close()

    let calculateScreenCoordinateFromIndex index width = 
        {i = index % width; j = index / width}

    let mapScreenCoordinateToWorldCoodinate screenCoordinate viewPlane : WorldCoordinate = 
        let scale = tan (System.Math.PI * 0.5 * float viewPlane.fov / 180.)
        let imageAspectRatio = viewPlane.screenWidth ./. viewPlane.screenHeight
        let invWidth = 1. / float viewPlane.screenWidth
        let invHeight = 1. / float viewPlane.screenHeight

        //float xx = (2 * ((x + 0.5) * invWidth) - 1) * scale * aspectratio; 
        //float yy = (1 - 2 * ((y + 0.5) * invHeight)) * scale; 
        let x = (2. * ((screenCoordinate.i .+ 0.5) * invWidth) - 1.) * imageAspectRatio * scale
        let y = (1. - 2. * ((screenCoordinate.j .+ 0.5) * invHeight)) * scale
        {x = x; y = y; z = -1.}

    let normalizeWorld (worldCoordinate, pixel:ScreenCoordinate) = 
        let length = sqrt (worldCoordinate.x * worldCoordinate.x + worldCoordinate.y * worldCoordinate.y + worldCoordinate.z * worldCoordinate.z)
        {dirX = worldCoordinate.x / length; dirY = worldCoordinate.y / length; dirZ = worldCoordinate.z / length; }, pixel
    
    let worldSubWorld point origin = 
        (point.x - origin.x, point.y - origin.y, point.z - origin.z)

    let dotProduct vector vector2 = 
        let x,y,z = vector
        let x2, y2, z2 = vector2
        x * x2 + y * y2 + z * z2

    let intersectSphere (camera:Camera) (ray:Direction) (sphere:Sphere) = 
        (*
            // geometric solution
         Vec3f L = center - orig; 
         float tca = L.dotProduct(dir); 
         if (tca < 0) return false; 
         float d2 = L.dotProduct(L) - tca * tca; 
         if (d2 > radius2) return false; 
         float thc = sqrt(radius2 - d2); 
         t0 = tca - thc; 
         t1 = tca + thc; 

         if (t0 > t1) std::swap(t0, t1); 
 
        if (t0 < 0) { 
            t0 = t1; // if t0 is negative, let's use t1 instead 
            if (t0 < 0) return false; // both t0 and t1 are negative 
        } 
 
        t = t0; 
        *)
        let l = worldSubWorld sphere.origin camera
        let tca = dotProduct l (ray.dirX, ray.dirY, ray.dirZ)
        let d2 = (dotProduct l l) - (tca * tca)
        let thc = sqrt (sphere.radius - d2)
        let t0 = tca - thc;
        let t1 = tca + thc;

        //need to refactor, see https://stackoverflow.com/questions/28720585/nested-if-statements-vs-pattern-matching-in-f
        match tca < 0. with
            | false -> 
                match d2 > sphere.radius with
                    | false -> 
                        match t1 < 0. && t0 < 0. with
                        | true -> None
                        | false ->
                            match t1 < 0. && t0 >=0. with
                                | true -> Some((Shape.Sphere sphere), t0)
                                | false -> 
                                    match t0 < 0. && t1 >=0. with
                                        | true -> Some((Shape.Sphere sphere), t1)
                                        | false ->
                                            match t0 >= 0. && t1 >= 0. with
                                                | false -> None //should never get here
                                                | true -> Some((Shape.Sphere sphere), if t1 < t0 then t1 else t0)
                        
                    | true -> None
            | true -> None
        
    
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

    let render viewPlane (scene:Scene) =
        let initPixels = 
            (List.init (viewPlane.screenHeight * viewPlane.screenWidth) 
                (fun idx -> {coordinate = (calculateScreenCoordinateFromIndex idx viewPlane.screenWidth); color = {r = 0; g = 0; b = 0}})):Image
                
        let renderedPixels = 
            initPixels
            |> List.mapi (fun idx pixel -> (mapScreenCoordinateToWorldCoodinate (calculateScreenCoordinateFromIndex idx viewPlane.screenWidth) viewPlane), pixel.coordinate)
            |> List.map normalizeWorld
            |> List.map (fun result -> castRay result scene)
            :Image

        renderedPixels

 