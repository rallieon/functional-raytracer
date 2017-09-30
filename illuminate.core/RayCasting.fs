namespace Illuminate

open FsAlg.Generic
open Illuminate.Types
open Illuminate.Operators
open Illuminate.Math
module Core = 

    let mapScreenCoordinateToWorldCoodinate screenCoordinate viewPlane : WorldCoordinate = 
        let scale = tan (deg2rad (viewPlane.fov .* 0.5))
        let imageAspectRatio = viewPlane.screenWidth ./. viewPlane.screenHeight
        let x = (2. * (screenCoordinate.i .+ 0.5) / (float viewPlane.screenWidth - 1.)) * imageAspectRatio * scale
        let y = (1. - 2. * (screenCoordinate.j .+ 0.5) / float viewPlane.screenHeight) * scale
        {x = x; y = y; z = -1.}
    
    let normalizeWorld worldCoordinate = 
        let length = sqrt (worldCoordinate.x * worldCoordinate.x + worldCoordinate.y * worldCoordinate.y + worldCoordinate.z * worldCoordinate.z)
        {dirX = worldCoordinate.x / length; dirY = worldCoordinate.y / length; dirZ = worldCoordinate.z / length; }

    