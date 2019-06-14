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

    let getLightIntensity light scene hitObj =
        let lightVector = getLightVector light hitObj
        let normalLightDir = getDirectionFromVector lightVector |> normalizeDirection |> convertNormalToDirection
        let lightDistance = dotProduct (lightVector, lightVector)
        let lightDirectionVector = convertDirectionToVector normalLightDir
        let LdotN = (dotProduct (lightDirectionVector, hitObj.normal))
        let normalIntensity = max 0. LdotN

        let inShadow = getHitPoint normalLightDir hitObj.shadowOrigin scene

        let tNearShadow = 
            match inShadow with
                | Some hit -> hit.t
                | None -> 0.

        //check if the ray hits an object AND make sure that object is between the light and the hitpoint
        match inShadow, tNearShadow * tNearShadow < lightDistance with
            | Some shadowHit, true -> 0.
            | _ ->
                match light with
                | PointLight l -> l.intensity * normalIntensity
                | SpotLight l -> l.intensity * normalIntensity

 