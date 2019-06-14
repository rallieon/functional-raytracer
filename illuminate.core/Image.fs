namespace Illuminate
open Illuminate.Types

module Image = 
    let createPPM image width height = 
        let wr = new System.IO.StreamWriter("test.ppm")
        wr.WriteLine "P3"
        wr.WriteLine("{0} {1}", width, height)  //use writeline instead of printf for performance
        wr.WriteLine "255"
        ignore (image |> List.map(fun pixel -> (wr.Write("{0} {1} {2} ", pixel.pixelColor.r |> int, pixel.pixelColor.g |> int, pixel.pixelColor.b |> int))))
        wr.Close()