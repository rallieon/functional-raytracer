namespace Illuminate
open Illuminate.Types
open Illuminate.Math
open Illuminate.Coordinate
open Illuminate.Intersection
open Illuminate.Hit

module Light = 
    let calculateHDotL (lightDirection: Direction, hitObj:HitPoint) = 
        let hitVector = hitObj.normal |> convertNormalToVector
        let inverseDirection = negativeDirection lightDirection |> convertDirectionToVector
        let value = dotProduct (hitVector, inverseDirection)
        max 0. value

    let illuminateDistantLight (light:DistantLight, hitObj:HitPoint) = 
        let HdotL = calculateHDotL(light.direction, hitObj)
        let attunedLuminosity = calculateColorIntensity(light.luminosity, light.intensity)
        let distanceAffectedLuminosity = {r = attunedLuminosity.r * HdotL; g = attunedLuminosity.g * HdotL; b = attunedLuminosity.b * HdotL}
        {lightDistance = infinity; lightDirection = light.direction; luminosity = distanceAffectedLuminosity}
    
    let illuminateSpotLight (light:SpotLight, hitObj:HitPoint) = 
        //TODO implement actual spotlight code
        let lightVector = (worldSubWorld hitObj.point light.origin)
        let r2 = dotProduct (lightVector, lightVector)
        let distance = sqrt r2
        let x,y,z = lightVector
        let normalLightDir = {dirX = x / distance; dirY = y / distance; dirZ = z /distance}
        let luminosity = calculateColorIntensity(light.luminosity, light.intensity)
        let attunedLuminosity = {r = luminosity.r / (4. * System.Math.PI * r2); g = luminosity.g / (4. * System.Math.PI * r2); b = luminosity.b / (4. * System.Math.PI * r2);}
        let HdotL = calculateHDotL(normalLightDir, hitObj)

        let distanceAffectedLuminosity = {r = attunedLuminosity.r * HdotL; g = attunedLuminosity.g * HdotL; b = attunedLuminosity.b * HdotL}

        {lightDistance = distance; lightDirection = normalLightDir; luminosity = distanceAffectedLuminosity }
    
    let illuminatePointLight (light:PointLight, hitObj:HitPoint) = 
        let lightVector = (worldSubWorld hitObj.point light.origin)
        let r2 = dotProduct (lightVector, lightVector)
        let distance = sqrt r2
        let x,y,z = lightVector
        let normalLightDir = {dirX = x / distance; dirY = y / distance; dirZ = z /distance}
        let luminosity = calculateColorIntensity(light.luminosity, light.intensity)
        let attenuation = (4. * System.Math.PI * r2)
        let attunedLuminosity = {r = luminosity.r / attenuation; g = luminosity.g / attenuation; b = luminosity.b / attenuation}
        let HdotL = calculateHDotL(normalLightDir, hitObj)

        let distanceAffectedLuminosity = {r = attunedLuminosity.r * HdotL; g = attunedLuminosity.g * HdotL; b = attunedLuminosity.b * HdotL}

        {lightDistance = distance; lightDirection = normalLightDir; luminosity = distanceAffectedLuminosity }

    let getLightIntensity light scene hitObj =
        let lightHit = 
            match light with
            | PointLight pl -> illuminatePointLight (pl, hitObj)
            | DistantLight dl -> illuminateDistantLight (dl, hitObj)
            | SpotLight sl -> illuminateSpotLight (sl, hitObj)

        let negativeDirection = negativeDirection lightHit.lightDirection
        
        //use the shadow origin to avoid shadow acne
        let inShadow = getHitPoint negativeDirection hitObj.shadowOrigin scene

        match inShadow with
            | Some shadowHit -> {r = 0.; g = 0.; b = 0.}
            | _ -> lightHit.luminosity