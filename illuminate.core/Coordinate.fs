namespace Illuminate
open Illuminate.Types
open Illuminate.Math
open Illuminate.MathTypes

module Coordinate = 

    let calculateScreenCoordinateFromIndex index width = 
        {i = index % width; j = index / width}

    let mapScreenCoordinateToWorldCoodinate (screenCoordinate:ScreenCoordinate, viewPlane:ViewPlane) : WorldCoordinate = 
        let scale = tan (float viewPlane.fov * 0.5 * System.Math.PI / 180.)
        let imageAspectRatio = float viewPlane.screenWidth / float viewPlane.screenHeight
        let invWidth = 1. / float viewPlane.screenWidth
        let invHeight = 1. / float viewPlane.screenHeight
        let x = (2. * ((float screenCoordinate.i + 0.5) * invWidth) - 1.) * imageAspectRatio * scale
        let y = (1. - 2. * ((float screenCoordinate.j + 0.5) * invHeight)) * scale
        {x = x; y = y; z = -1.}