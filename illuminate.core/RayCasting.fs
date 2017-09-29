namespace Illuminate

open FsAlg.Generic
open Illuminate.Types
open Illuminate.Operators

module Core = 

    let mapScreenCoordinateToWorldCoodinate screenCoordinate viewPlane : WorldCoordinate = 
        let x = (screenCoordinate.x ./. viewPlane.screenWidth) * float viewPlane.screenHeight
        let y = (screenCoordinate.y ./. viewPlane.screenHeight) * float viewPlane.screenWidth
        {x = x; y = y; z = 0.}
