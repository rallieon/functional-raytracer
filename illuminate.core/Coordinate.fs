namespace Illuminate
open Illuminate.Types
open Illuminate.Operators
open Illuminate.Math
open Illuminate.MathTypes

module Coordinate = 

    let calculateScreenCoordinateFromIndex index width = 
        {i = index % width; j = index / width}

    let mapScreenCoordinateToWorldCoodinate (screenCoordinate:ScreenCoordinate, viewPlane:ViewPlane) : WorldCoordinate = 
        let scale = tan (float viewPlane.fov * 0.5 * System.Math.PI / 180.)
        let imageAspectRatio = viewPlane.screenWidth ./. viewPlane.screenHeight
        let invWidth = 1. / float viewPlane.screenWidth
        let invHeight = 1. / float viewPlane.screenHeight

        //float xx = (2 * ((x + 0.5) * invWidth) - 1) * scale * aspectratio; 
        //float yy = (1 - 2 * ((y + 0.5) * invHeight)) * scale; 
        let x = (2. * ((screenCoordinate.i .+ 0.5) * invWidth) - 1.) * imageAspectRatio * scale
        let y = (1. - 2. * ((screenCoordinate.j .+ 0.5) * invHeight)) * scale
        {x = x; y = y; z = -1.}