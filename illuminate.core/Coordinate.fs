namespace Illuminate
open Illuminate.Types
open Illuminate.Math

module Coordinate = 

    let calculateScreenCoordinateFromIndex index width = 
        {i = index % width; j = index / width}
    
    let calculateIndexFromPixelCoordinate pixel scene = 
        scene.width * pixel.coordinate.j + pixel.coordinate.i

    let mapScreenCoordinateToWorldCoodinate screenCoordinate viewPlane = 
        let imageAspectRatio = float viewPlane.width / float viewPlane.height
        let scale = tan (float viewPlane.fov * 0.5 * System.Math.PI / 180.)
        let invWidth = 1. / float viewPlane.width
        let invHeight = 1. / float viewPlane.height
        let x = (2. * ((float screenCoordinate.i + 0.5) * invWidth) - 1.) * imageAspectRatio * scale
        let y = (1. - 2. * ((float screenCoordinate.j + 0.5) * invHeight)) * scale
        {x = x; y = y; z = -1.}