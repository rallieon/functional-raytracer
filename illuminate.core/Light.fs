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
        max 0. (dotProduct (hitVector, inverseDirection))

    let illuminateDistantLight (light:DistantLight, hitObj:HitPoint) = 
        let HdotL = calculateHDotL(light.direction, hitObj)
        let attunedLuminosity = calculateColorIntensity(light.luminosity, light.intensity)
        let distanceAffectedLuminosity = {r = attunedLuminosity.r * HdotL; g = attunedLuminosity.g * HdotL; b = attunedLuminosity.b * HdotL}
        {lightDistance = infinity; lightDirection = light.direction; luminosity = distanceAffectedLuminosity}
    
    let illuminateSpotLight (light:SpotLight, hitObj:HitPoint) = 
        //TODO implement actual spotlight code
        let lightVector = (worldSubWorld light.origin hitObj.point)
        let normalLightDir = getDirectionFromVector lightVector |> normalizeDirection |> convertNormalToDirection
        let r2 = dotProduct (lightVector, lightVector)
        let distance = sqrt r2
        let luminosity = calculateColorIntensity(light.luminosity, light.intensity)
        let attunedLuminosity = {r = luminosity.r / (4. * System.Math.PI * r2); g = luminosity.g / (4. * System.Math.PI * r2); b = luminosity.b / (4. * System.Math.PI * r2);}
        let HdotL = calculateHDotL(normalLightDir, hitObj)
        
        let distanceAffectedLuminosity = {r = attunedLuminosity.r * HdotL; g = attunedLuminosity.g * HdotL; b = attunedLuminosity.b * HdotL}

        {lightDistance = distance; lightDirection = normalLightDir; luminosity = distanceAffectedLuminosity }
    
    let illuminatePointLight (light:PointLight, hitObj:HitPoint) = 
        let lightVector = (worldSubWorld light.origin hitObj.point)
        let normalLightDir = getDirectionFromVector lightVector |> normalizeDirection |> convertNormalToDirection
        let r2 = dotProduct (lightVector, lightVector)
        let distance = sqrt r2
        let luminosity = calculateColorIntensity(light.luminosity, light.intensity)
        let attunedLuminosity = {r = luminosity.r / (4. * System.Math.PI * r2); g = luminosity.g / (4. * System.Math.PI * r2); b = luminosity.b / (4. * System.Math.PI * r2);}
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
        let inShadow = getHitPoint negativeDirection hitObj.shadowOrigin scene

        //check if the ray hits an object AND make sure that object is between the light and the hitpoint
        match inShadow with
            | Some shadowHit -> {r = 0.; g = 0.; b = 0.}
            | _ ->
                match light with
                | PointLight l -> l.luminosity
                | SpotLight l -> l.luminosity
                | DistantLight l -> l.luminosity