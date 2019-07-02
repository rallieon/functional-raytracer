namespace Illuminate.Framework
open Illuminate.Framework.Types
open Illuminate.Framework.Math
open Illuminate.Material.Material
open Illuminate.Framework.Hit
open Illuminate.Framework.Intersection
open FsAlg.Generic

module Ray = 
    let castRay scene (ray,pixel)  =
        debug pixel scene
        
        let hit = trace ray scene.camera.cameraOrigin scene
        let hitColor = 
            match hit with
                | Some point -> getHitColor point scene
                | None -> {r = 0.; g = 0.; b = 0.}

        {coordinate = pixel; pixelColor = hitColor}