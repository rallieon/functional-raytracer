namespace Illuminate.Framework
open Illuminate.Framework.Types
open Illuminate.Framework.Math
open FsAlg.Generic

module Coordinate = 

    let calculateScreenCoordinateFromIndex index width = 
        {i = index % width; j = index / width}
    
    let calculateIndexFromPixelCoordinate pixel scene = 
        scene.width * pixel.coordinate.j + pixel.coordinate.i

    let mapScreenCoordinateToWorldCoodinate screenCoordinate scene = 
        let imageAspectRatio = float scene.width / float scene.height
        let scale = tan (float scene.fov * 0.5 * System.Math.PI / 180.)
        let invWidth = 1. / float scene.width
        let invHeight = 1. / float scene.height
        let x = (2. * ((float screenCoordinate.i + 0.5) * invWidth) - 1.) * imageAspectRatio * scale
        let y = (1. - 2. * ((float screenCoordinate.j + 0.5) * invHeight)) * scale
        vector [x; y; -1.;]
    
    let buildCoordinate scene idx pixel =
        let screenCoord = calculateScreenCoordinateFromIndex idx scene.width
        (mapScreenCoordinateToWorldCoodinate screenCoord scene), pixel.coordinate