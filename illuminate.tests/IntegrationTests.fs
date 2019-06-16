namespace Illuminate

open Xunit
open Illuminate.Types
open Illuminate.Render
open Illuminate.Image
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
        let index = getIndex 57 240 scene
        let pixel = t.[index]

        Assert.Equal(235.09, pixel.pixelColor.r, 2)
        Assert.Equal(0., pixel.pixelColor.g)
        Assert.Equal(0., pixel.pixelColor.b)
    
    [<Fact>]
    let ``can render a ray hitting blue sphere`` () =
        let scene = getScene
        let t = render scene
        let index = getIndex 394 236 scene
        let pixel = t.[index]

        Assert.Equal(0., pixel.pixelColor.r)
        Assert.Equal(0., pixel.pixelColor.g)
        Assert.Equal(205.87, pixel.pixelColor.b, 2)
    
    [<Fact>]
    let ``can render a ray hitting blue sphere in shadow`` () =
        let scene = getScene
        let t = render scene
        let index = getIndex 345 240 scene
        let pixel = t.[index]

        Assert.Equal(0., pixel.pixelColor.r, 2)
        Assert.Equal(0., pixel.pixelColor.g)
        Assert.Equal(0., pixel.pixelColor.b)