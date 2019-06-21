namespace Illuminate
open Illuminate.Types
open Illuminate.Math
open Illuminate.Coordinate
open Illuminate.Intersection
open Illuminate.Hit

module Light = 
    let calculateHDotL lightDirection hitObj = 
        let hitVector = hitObj.normal |> normalizeVector |> convertNormalToVector
        let inverseDirection = lightDirection |> invertDirection |> convertDirectionToVector
        let value = dotProduct (hitVector, inverseDirection)
        max 0. value

    let illuminateDistantLight light hitObj = 
        let HdotL = calculateHDotL light.distantLightDirection hitObj
        let luminosity = calculateColorIntensity light.luminosity light.intensity

        //calculate the light absorbed at the hit point luminosity
        let hitPointLuminosity = {r = luminosity.r * HdotL; g = luminosity.g * HdotL; b = luminosity.b * HdotL}
        {lightDistance = infinity; lightDirection = light.distantLightDirection; luminosity = hitPointLuminosity}
    
    let illuminateSpotLight (light:SpotLight, hitObj:HitPoint) = 
        //TODO implement actual spotlight code
        {lightDistance = 0.; lightDirection = {dirX = 0.; dirY = 0.; dirZ = 0.}; luminosity = {r = 0.; b = 0.; g = 0.} }
    
    let illuminatePointLight light hitObj = 
        let lightVector = (worldSubWorld hitObj.point light.pointOrigin)
        let r2 = dotProduct (lightVector, lightVector)
        let distance = sqrt r2
        let x,y,z = lightVector

        //normalize the ligth direction based on distance
        let normalLightDir = {dirX = x / distance; dirY = y / distance; dirZ = z /distance}

        //get initial luminosity
        let luminosity = calculateColorIntensity light.luminosity light.intensity

        //calculate the distance attuned luminosity
        let attenuation = (4. * System.Math.PI * r2)
        let distanceAffectedLuminosity = {r = luminosity.r / attenuation; g = luminosity.g / attenuation; b = luminosity.b / attenuation}

        //calculate the light absorbed at the hit point luminosity
        let HdotL = calculateHDotL normalLightDir hitObj
        let hitPointLuminosity = {r = distanceAffectedLuminosity.r * HdotL; g = distanceAffectedLuminosity.g * HdotL; b = distanceAffectedLuminosity.b * HdotL}

        {lightDistance = distance; lightDirection = normalLightDir; luminosity = hitPointLuminosity }

    let getLightIntensity light scene hitObj =
        let lightHit = 
            match light with
            | PointLight pl -> illuminatePointLight pl hitObj
            | DistantLight dl -> illuminateDistantLight dl hitObj
            | SpotLight sl -> illuminateSpotLight (sl, hitObj)

        let invertedDirection = invertDirection lightHit.lightDirection
        
        //use the shadow origin to avoid shadow acne
        let inShadow = getHitPoint invertedDirection hitObj.shadowOrigin scene

        match inShadow with
            | Some shadowHit -> {r = 0.; g = 0.; b = 0.}
            | _ -> lightHit.luminosity