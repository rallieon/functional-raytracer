namespace Illuminate
open Illuminate.Types
open Illuminate.Math
open Illuminate.Coordinate
open Illuminate.Intersection
open Illuminate.Hit

module Light = 
    let getLightVector light hitObj = 
        match light with
            | PointLight l -> worldSubWorld l.origin hitObj.point
            | SpotLight l -> worldSubWorld l.origin hitObj.point

    let hitLight light scene hitObj =
        let lightVector = getLightVector light hitObj
        let lightDirection = getDirectionFromVector lightVector
        let normalLightDir = normalizeDirection(lightDirection) |> convertNormalToDirection
        let lightDistance = dotProduct (lightVector, lightVector)
        let lightHit = getHitPoint lightDirection hitObj.point scene
        match lightHit with
            | Some hit -> Some {lightDirection = normalLightDir; lightDistance = lightDistance; lightHit = lightHit; light = light}
            | None -> None

    let getLightIntensity lightHit scene hitObj =
        let lightDirectionVector = convertDirectionToVector lightHit.lightDirection
        let LdotN = (dotProduct (lightDirectionVector, hitObj.normal))
        let normalIntensity = max 0. LdotN
        let inShadow = getHitPoint lightHit.lightDirection hitObj.point scene

        let tNearShadow = 
            match inShadow with
                | Some hit -> hit.t
                | None -> 0.

        //check if the ray hits an object AND make sure that object is between the light and the hitpoint
        match inShadow, tNearShadow * tNearShadow < lightHit.lightDistance with
            | Some shadowHit, true -> 0.
            | _ ->
                match lightHit.light with
                | PointLight l -> l.intensity * normalIntensity
                | SpotLight l -> l.intensity * normalIntensity

 