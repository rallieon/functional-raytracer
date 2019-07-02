namespace Illuminate.Lighting
open Illuminate.Framework.Types
open Illuminate.Framework.Math
open Illuminate.Framework.Coordinate
open Illuminate.Framework.Intersection
open Illuminate.Framework.Hit
open FsAlg.Generic

module Light = 
    let calculateColorIntensity color luminosity = 
        {r = color.r * luminosity; g = color.g * luminosity; b = color.b * luminosity}
        
    let calculateHDotL lightDirection hitObj = 
        let hitVector = hitObj.normal |> Vector.unitVector
        let inverseDirection = -lightDirection
        let value = hitVector * inverseDirection
        max 0. value

    let illuminateDistantLight light hitObj = 
        let HdotL = calculateHDotL light.distantLightDirection hitObj
        let luminosity = calculateColorIntensity light.luminosity light.intensity

        //calculate the light absorbed at the hit point luminosity
        let hitPointLuminosity = {r = luminosity.r * HdotL; g = luminosity.g * HdotL; b = luminosity.b * HdotL}
        {lightDistance = infinity; lightDirection = light.distantLightDirection; luminosity = hitPointLuminosity}
    
    let illuminateSpotLight (light:SpotLight, hitObj:HitPoint) = 
        //TODO implement actual spotlight code
        {lightDistance = 0.; lightDirection = vector [0.; 0.; 0.]; luminosity = {r = 0.; b = 0.; g = 0.} }
    
    let illuminatePointLight light hitObj = 
        let lightVector = hitObj.point - light.pointOrigin
        let r2 = lightVector * lightVector
        let distance = sqrt r2

        //normalize the light direction based on distance
        let normalLightDir = vector [lightVector.[0] / distance; lightVector.[1] / distance; lightVector.[2] / distance]

        //get initial luminosity
        let luminosity = calculateColorIntensity light.luminosity light.intensity

        //calculate the distance attuned luminosity
        let attenuation = (4. * System.Math.PI * r2)
        let distanceAffectedLuminosity = {r = luminosity.r / attenuation; g = luminosity.g / attenuation; b = luminosity.b / attenuation}

        //calculate the light absorbed at the hit point luminosity
        let HdotL = calculateHDotL normalLightDir hitObj
        let hitPointLuminosity = {r = distanceAffectedLuminosity.r * HdotL; g = distanceAffectedLuminosity.g * HdotL; b = distanceAffectedLuminosity.b * HdotL}

        {lightDistance = distance; lightDirection = normalLightDir; luminosity = hitPointLuminosity }

    let getLightIntensity scene hitObj light =
        let lightHit = 
            match light with
            | PointLight pl -> illuminatePointLight pl hitObj
            | DistantLight dl -> illuminateDistantLight dl hitObj
            | SpotLight sl -> illuminateSpotLight (sl, hitObj)

        let invertedDirection = -lightHit.lightDirection
        
        //use the shadow origin to avoid shadow acne
        let inShadow = trace invertedDirection hitObj.shadowOrigin scene

        match inShadow with
            | Some shadowHit -> 
                //make sure its no the same object
                match shadowHit.shape = hitObj.shape with
                    | true -> lightHit.luminosity
                    | false -> {r = 0.; g = 0.; b = 0.}
            | _ -> lightHit.luminosity