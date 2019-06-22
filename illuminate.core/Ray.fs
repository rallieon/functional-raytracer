namespace Illuminate
open Illuminate.Types
open Illuminate.Math
open Illuminate.Material
open Illuminate.Hit

module Ray = 
    let castRay scene (ray,pixel)  =
        (*
            Stupid debugging needed
        *)
        if pixel.i = 430 && pixel.j = 310
        then 
            printf ""
        
        
        let hit = getHitPoint ray scene.camera scene
        let hitColor = 
            match hit with
                | Some point -> getHitColor point scene
                | None -> {r = 0.; g = 0.; b = 0.}

        {coordinate = pixel; pixelColor = hitColor}