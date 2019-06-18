namespace Illuminate
open Illuminate.Types
open SkiaSharp
open System.IO

module SceneWriter = 
    let castByte (col:float) =
        if col > 255. then 255uy else byte col

    let convertPixel pixel =
        new SkiaSharp.SKColor( (pixel.pixelColor.r |> castByte), (pixel.pixelColor.g |> castByte), (pixel.pixelColor.b |> castByte))

    let writeScene (image:Image, scene:Scene, outputPath:string) =
        let bitmap = new SKBitmap(scene.width, scene.height)
        let skiaPixels = image |> List.map convertPixel
        bitmap.Pixels <- List.toArray skiaPixels
        let image = SKImage.FromBitmap bitmap
        let data = image.Encode(SKEncodedImageFormat.Jpeg, 99)  //there is a bug in the jpeg encoder. do not make it 100 or you might run into unexplained encoding errors.
        let stream = File.OpenWrite(outputPath)
        data.SaveTo(stream)
        0