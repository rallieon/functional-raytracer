namespace Illuminate
open Illuminate.Types
open SkiaSharp
open System.IO

module SceneWriter = 
    let convertPixel pixel =
        new SkiaSharp.SKColor( (byte pixel.pixelColor.r), (byte pixel.pixelColor.g), (byte pixel.pixelColor.b))

    let writeScene (image:Image, scene:Scene) =
        let bitmap = new SKBitmap(scene.width, scene.height)
        let skiaPixels = image |> List.map convertPixel
        bitmap.Pixels <- List.toArray skiaPixels
        let image = SKImage.FromBitmap bitmap
        let data = image.Encode(SKEncodedImageFormat.Jpeg, 100)
        let timeNow = System.DateTime.Now.ToString("yyyy-MM-dd HH-mm-ss")
        let fileName = "meta\\images\\" + timeNow + ".jpeg"

        let stream = File.OpenWrite(fileName)
        data.SaveTo(stream)
        0