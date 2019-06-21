namespace Illuminate
open Illuminate.Types
open Illuminate.Math
open Illuminate.Material
open Illuminate.Hit

module Ray = 
    let castRay (ray:Direction,pixel:ScreenCoordinate) (scene:Scene) =
        (*
            Stupid debugging needed
        *)
        if pixel.i = 350 && pixel.j = 200
        then 
            printf ""
        
        
        let hit = getHitPoint ray scene.camera scene
        let hitColor = 
            match hit with
                | Some point -> getHitColor point scene
                | None -> {r = 0.; g = 0.; b = 0.}

        {coordinate = pixel; pixelColor = hitColor}