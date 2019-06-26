namespace Illuminate

open Xunit
open Illuminate.Framework.Types
open Illuminate.Framework.Render
open Illuminate.Interface.SceneReader
open Illuminate.Framework.Coordinate

module IntegrationTests = 
    let getScene =
        readScene "../../../../meta/scenes/integrationtest.json"

    let getIndex x y scene =
        calculateIndexFromPixelCoordinate {coordinate = {i = x; j = y}; pixelColor = {r = 0.; b = 0.; g = 0.;}} scene

    let scene = getScene
    let t = render scene

    [<Fact>]
    let ``can render an image of appropriate size`` () =
        Assert.Equal(307200, t.Length)
    
    [<Fact>]
    let ``can render a ray miss`` () =
        let pixel = t.[0]
        Assert.Equal(0., pixel.pixelColor.r)
        Assert.Equal(0., pixel.pixelColor.g)
        Assert.Equal(0., pixel.pixelColor.b)
    
    [<Fact>]
    let ``can render a ray hitting red sphere`` () =
        let index = getIndex 130 220 scene
        let pixel = t.[index]

        Assert.Equal(128.7, pixel.pixelColor.r, 2)
        Assert.Equal(0., pixel.pixelColor.g)
        Assert.Equal(0., pixel.pixelColor.b)
    
    [<Fact>]
    let ``can render a ray hitting blue sphere`` () =
        let index = getIndex 520 220 scene
        let pixel = t.[index]

        Assert.Equal(0., pixel.pixelColor.r)
        Assert.Equal(0., pixel.pixelColor.g)
        Assert.Equal(119.5, pixel.pixelColor.b, 2)
    
    [<Fact>]
    let ``can render a ray hitting blue plane`` () =
        let index = getIndex 75 340 scene
        let pixel = t.[index]

        Assert.Equal(0., pixel.pixelColor.r)
        Assert.Equal(0., pixel.pixelColor.g)
        Assert.Equal(86.41, pixel.pixelColor.b, 2)
    
    [<Fact>]
    let ``can render a ray hitting blue plane in shadow from red sphere`` () =
        let index = getIndex 70 290 scene
        let pixel = t.[index]

        Assert.Equal(0., pixel.pixelColor.r, 2)
        Assert.Equal(0., pixel.pixelColor.g)
        Assert.Equal(0., pixel.pixelColor.b, 2)
    
    [<Fact>]
    let ``can render a ray hitting a blue triangle`` () =
        let index = getIndex 350 200 scene
        let pixel = t.[index]

        Assert.Equal(0., pixel.pixelColor.r, 2)
        Assert.Equal(0., pixel.pixelColor.g)
        Assert.Equal(97.54, pixel.pixelColor.b, 2)
    
    [<Fact>]
    let ``can render a ray hitting a blue triangles shadow`` () =
        let index = getIndex 425 305 scene
        let pixel = t.[index]

        Assert.Equal(0., pixel.pixelColor.r, 2)
        Assert.Equal(0., pixel.pixelColor.g)
        Assert.Equal(0., pixel.pixelColor.b)