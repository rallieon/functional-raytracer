namespace Illuminate
open Illuminate.Types
open Illuminate.Math
open Illuminate.Material
open Illuminate.Hit
open Illuminate.Intersection

module Ray = 
    let castRay scene (ray,pixel)  =
        debug pixel scene
        
        let hit = getHitPoint ray scene.camera scene
        let hitColor = 
            match hit with
                | Some point -> getHitColor point scene
                | None -> {r = 0.; g = 0.; b = 0.}

        {coordinate = pixel; pixelColor = hitColor}