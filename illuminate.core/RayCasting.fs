namespace Illuminate
open Illuminate.Types
open Illuminate.Operators
open Illuminate.Math
open System

module Core = 

    let createPPM (image:Image) width height = 
        let wr = new System.IO.StreamWriter("test.ppm")
        wr.WriteLine "P3"
        wr.WriteLine("{0} {1}", width, height)  //use writeline instead of printf for performance
        wr.WriteLine "255"
        ignore (image |> List.map(fun pixel -> (wr.Write("{0} {1} {2} ", pixel.color.r, pixel.color.g, pixel.color.b))))
        wr.Close()

    let calculateScreenCoordinateFromIndex index width = 
        {i = index / width; j = index % width}

    let render viewPlane (scene:Scene) =
        let pixels = 
            (List.init (viewPlane.screenHeight * viewPlane.screenWidth) 
                (fun idx -> {coordinate = (calculateScreenCoordinateFromIndex idx viewPlane.screenWidth); 
                    color = {r = 0; g = 0; b = 0}}))
                :Image
        pixels

    let mapScreenCoordinateToWorldCoodinate screenCoordinate viewPlane : WorldCoordinate = 
        let scale = tan (deg2rad (viewPlane.fov .* 0.5))
        let imageAspectRatio = viewPlane.screenWidth ./. viewPlane.screenHeight
        let x = (2. * (screenCoordinate.i .+ 0.5) / (float viewPlane.screenWidth - 1.)) * imageAspectRatio * scale
        let y = (1. - 2. * (screenCoordinate.j .+ 0.5) / float viewPlane.screenHeight) * scale
        {x = x; y = y; z = -1.}
    
    let normalizeWorld worldCoordinate = 
        let length = sqrt (worldCoordinate.x * worldCoordinate.x + worldCoordinate.y * worldCoordinate.y + worldCoordinate.z * worldCoordinate.z)
        {dirX = worldCoordinate.x / length; dirY = worldCoordinate.y / length; dirZ = worldCoordinate.z / length; }

 