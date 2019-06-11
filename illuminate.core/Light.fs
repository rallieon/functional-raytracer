namespace Illuminate
open Illuminate.Types
open Illuminate.Operators
open Illuminate.Math
open Illuminate.Coordinate
open Illuminate.Intersection
open Illuminate.Hit

module Light = 
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

 