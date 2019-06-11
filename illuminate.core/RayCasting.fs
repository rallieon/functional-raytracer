namespace Illuminate
open Illuminate.Types
open Illuminate.Operators
open Illuminate.Math
open Illuminate.Coordinate
open Illuminate.Intersection

module Core = 
    let getHitPoint ray scene : HitPoint option =
        scene.shapes
            |> List.map (fun shape -> (intersect scene.camera ray shape))
            |> List.minBy (fun intersection -> 
                match intersection with
                    | Some result -> Some result
                    | None -> None)

    let getLightVector light hitObj = 
        let _, _, hitCoordinate = hitObj
        match light with
            | PointLight l -> worldSubWorld l.origin hitCoordinate
            | SpotLight l -> worldSubWorld l.origin hitCoordinate

    let hitLight (light:Light, scene:Scene, hitObj:HitPoint) : LightHitPoint option =
        let lightVector = getLightVector light hitObj
        let lightDirection = getDirectionFromVector lightVector
        let lightDistance = dotProduct (lightVector, lightVector)
        let lightHit = getHitPoint lightDirection scene
        match lightHit with
            | Some hit -> Some {lightDirection = lightDirection; lightDistance = lightDistance; lightHit = lightHit; light = light}
            | None -> None

    let getLightIntensity lightHit scene =
        let normalLightDirection = normalizeDirection(lightHit.lightDirection) |> convertNormalToDirection
        let lightDirectionVector = convertDirectionToVector lightHit.lightDirection
        let LdotN = max 0. (dotProduct (lightDirectionVector, lightDirectionVector))
        let inShadow = getHitPoint normalLightDirection scene

        match inShadow with
            | Some shadowHit -> 0.   //it hit an object before it hit the light...need to fix bug here where what if the object is behind the light. check distance!
            | None -> 
                match lightHit.light with
                | PointLight l -> l.intensity * LdotN
                | SpotLight l -> l.intensity * LdotN

    let getHitColor (hitObj:HitPoint, scene:Scene) = 
                (*
        for (uint32_t i = 0; i < lights.size(); ++i) { 
                    Vec3f lightDir = lights[i]->position - hitPoint; 
                    // square of the distance between hitPoint and the light
                    float lightDistance2 = dotProduct(lightDir, lightDir); 
                    lightDir = normalize(lightDir); 
                    float LdotN = std::max(0.f, dotProduct(lightDir, N)); 
                    Object *shadowHitObject = nullptr; 
                    float tNearShadow = kInfinity; 
                    // is the point in shadow, and is the nearest occluding object closer to the object than the light itself?
                    bool inShadow = trace(shadowPointOrig, lightDir, objects, tNearShadow, index, uv, &shadowHitObject) && 
                        tNearShadow * tNearShadow < lightDistance2; 
                    lightAmt += (1 - inShadow) * lights[i]->intensity * LdotN; 
                    Vec3f reflectionDirection = reflect(-lightDir, N); 
                    specularColor += powf(std::max(0.f, -dotProduct(reflectionDirection, dir)), hitObject->specularExponent) * lights[i]->intensity; 
                } 
        *)
        let shape, _, _ = hitObj
        let shapeColor = 
            match shape with
                | Sphere s -> s.color
                | Plane p -> p.color

        scene.lights
            |> List.map (fun light -> hitLight(light, scene, hitObj))
            |> List.map (
                fun lightHit -> 
                    match lightHit with
                        | Some hit -> getLightIntensity hit scene
                        | None -> 0.
                )
            |> List.fold(fun color intensity -> 
                    let r = color.r + shapeColor.r * intensity
                    let g = color.g + shapeColor.g * intensity
                    let b = color.b + shapeColor.b * intensity
                    {r = r; g = g; b = b}
               ) {r = 0.; g = 0.; b = 0.}

    let castRay (ray:Direction,pixel:ScreenCoordinate) (scene:Scene) =
        let hit = getHitPoint ray scene
        let hitColor = 
            match hit with
                | Some point -> getHitColor (point, scene)
                | None -> {r = 0.; g = 0.; b = 0.}

        {coordinate = pixel; color = hitColor}

    let render scene =
        let initPixels = 
            (List.init (scene.height * scene.width) 
                (fun idx -> {coordinate = (calculateScreenCoordinateFromIndex idx scene.width); color = {r = 0.; g = 0.; b = 0.}})):Image
                
        let renderedPixels = 
            initPixels
            |> List.mapi (fun idx pixel -> (mapScreenCoordinateToWorldCoodinate ((calculateScreenCoordinateFromIndex idx scene.width), {screenWidth = scene.width; screenHeight = scene.height; fov = scene.fov })), pixel.coordinate)
            |> List.map normalizeWorld
            |> List.map (fun result -> castRay result scene)
            :Image

        renderedPixels

 