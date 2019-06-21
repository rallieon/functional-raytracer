namespace Illuminate

open Xunit
open Illuminate.Types
open Illuminate.Render
open Illuminate.SceneReader
open Illuminate.Coordinate

module IntegrationTests = 
    let getScene =
        readScene "../../../../meta/scenes/integrationtest.json"

    let getIndex x y scene =
        calculateIndexFromPixelCoordinate {coordinate = {i = x; j = y}; pixelColor = {r = 0.; b = 0.; g = 0.;}} scene

    [<Fact>]
    let ``can render an image of appropriate size`` () =
        let scene = getScene
        let t = render scene
        Assert.Equal(307200, t.Length)
    
    [<Fact>]
    let ``can render a ray miss`` () =
        let scene = getScene
        let t = render scene
        let pixel = t.[0]
        Assert.Equal(0., pixel.pixelColor.r)
        Assert.Equal(0., pixel.pixelColor.g)
        Assert.Equal(0., pixel.pixelColor.b)
    
    [<Fact>]
    let ``can render a ray hitting red sphere`` () =
        let scene = getScene
        let t = render scene
        let index = getIndex 130 220 scene
        let pixel = t.[index]

        Assert.Equal(148.37, pixel.pixelColor.r, 2)
        Assert.Equal(0., pixel.pixelColor.g)
        Assert.Equal(0., pixel.pixelColor.b)
    
    [<Fact>]
    let ``can render a ray hitting blue sphere`` () =
        let scene = getScene
        let t = render scene
        let index = getIndex 520 220 scene
        let pixel = t.[index]

        Assert.Equal(0., pixel.pixelColor.r)
        Assert.Equal(0., pixel.pixelColor.g)
        Assert.Equal(128.51, pixel.pixelColor.b, 2)
    
    [<Fact>]
    let ``can render a ray hitting blue plane`` () =
        let scene = getScene
        let t = render scene
        let index = getIndex 75 340 scene
        let pixel = t.[index]

        Assert.Equal(0., pixel.pixelColor.r)
        Assert.Equal(0., pixel.pixelColor.g)
        Assert.Equal(97.2, pixel.pixelColor.b, 2)
    
    [<Fact>]
    let ``can render a ray hitting blue plane in shadow from red sphere`` () =
        let scene = getScene
        let t = render scene
        let index = getIndex 70 290 scene
        let pixel = t.[index]

        Assert.Equal(0., pixel.pixelColor.r, 2)
        Assert.Equal(0., pixel.pixelColor.g)
        Assert.Equal(0., pixel.pixelColor.b)