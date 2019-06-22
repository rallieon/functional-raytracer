namespace Illuminate
open Illuminate.Types
open SixLabors.ImageSharp
open SixLabors.ImageSharp.PixelFormats
open System.IO

module SceneWriter = 
    let castByte col =
            if col > 255. then byte 255 else byte col

    let writeScene image scene outputPath =
        let convertPixel pixel =
            new Rgb24( (pixel.pixelColor.r |> castByte), (pixel.pixelColor.g |> castByte), (pixel.pixelColor.b |> castByte))

        let colorMap = image |> List.map convertPixel |> List.toArray
        let bitmap = Image.LoadPixelData(colorMap,scene.width, scene.height)
        let stream = File.OpenWrite(outputPath)
        bitmap.SaveAsJpeg(stream)
        0