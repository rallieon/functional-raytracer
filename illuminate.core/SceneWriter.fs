namespace Illuminate
open Illuminate.Types
open SkiaSharp
open System.IO

module SceneWriter = 
    let castByte (col:float) =
            if col > 255. then 255uy else byte col

    let writeScenePPM (image:Image, scene:Scene, outputPath:string) = 
        let wr = new System.IO.StreamWriter(outputPath)
        wr.WriteLine "P3"
        wr.WriteLine("{0} {1}", scene.width, scene.height)  //use writeline instead of printf for performance
        wr.WriteLine "255"
        ignore (image |> List.map(fun pixel -> (wr.Write("{0} {1} {2} ",  (pixel.pixelColor.r |> castByte), (pixel.pixelColor.g |> castByte), (pixel.pixelColor.b |> castByte)))))
        wr.Close()
        0

    let writeScene (image:Image, scene:Scene, outputPath:string) =
        let convertPixel pixel =
            new SkiaSharp.SKColor( (pixel.pixelColor.r |> castByte), (pixel.pixelColor.g |> castByte), (pixel.pixelColor.b |> castByte))

        let bitmap = new SKBitmap(scene.width, scene.height)
        let skiaPixels = image |> List.map convertPixel
        bitmap.Pixels <- List.toArray skiaPixels
        let image = SKImage.FromBitmap bitmap
        let data = image.Encode(SKEncodedImageFormat.Jpeg, 90)  //there is a bug in the jpeg encoder. do not make it 100 or you might run into unexplained encoding errors.
        let stream = File.OpenWrite(outputPath)
        data.SaveTo(stream)
        0