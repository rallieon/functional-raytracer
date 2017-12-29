namespace Illuminate

open Xunit
open Illuminate.Math
open Illuminate.Types
open Illuminate.Core

module Tests = 

    [<Fact>]
    let ``can convert degrees to radians`` () =
        let angle = 20.
        let convert = deg2rad angle
        Assert.Equal(0.35, convert, 2)

    [<Fact>]
    let ``can convert bottom left edge screen coordinate to world coordinate`` () =
        let screenCoordinate = {i = 0; j = 0}
        let rasterPlane = {screenWidth = 100; screenHeight = 100; fov = 90}
        let conversion = mapScreenCoordinateToWorldCoodinate screenCoordinate rasterPlane
        Assert.Equal(-0.99, conversion.x, 2)
        Assert.Equal(0.99, conversion.y, 2)
    [<Fact>]
    let ``can convert top right edge screen coordinate to world coordinate`` () =
        let screenCoordinate = {i = 100; j = 100}
        let rasterPlane = {screenWidth = 100; screenHeight = 100; fov = 90}
        let conversion = mapScreenCoordinateToWorldCoodinate screenCoordinate rasterPlane
        Assert.Equal(1.01, conversion.x, 2)
        Assert.Equal(-1.01, conversion.y, 2)
    [<Fact>]
    let ``can convert middle screen coordinate to world coordinate`` () =
        let screenCoordinate = {i = 50; j = 50}
        let rasterPlane = {screenWidth = 100; screenHeight = 100; fov = 90}
        let conversion = mapScreenCoordinateToWorldCoodinate screenCoordinate rasterPlane
        Assert.Equal(0.01, conversion.x, 2)
        Assert.Equal(-0.01, conversion.y, 2)