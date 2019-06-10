namespace Illuminate

open Xunit
open Illuminate.Math
open Illuminate.Types
open Illuminate.Core
open Illuminate.Coordinate

module Tests = 

    [<Fact>]
    let ``can convert degrees to radians`` () =
        let angle = 20.
        let convert = deg2rad angle
        Assert.Equal(0.35, convert, 2)

    [<Fact>]
    let ``can convert top left edge screen coordinate to world coordinate`` () =
        let screenCoordinate = {i = 0; j = 0}
        let rasterPlane = {screenWidth = 640; screenHeight = 480; fov = 90}
        let conversion = mapScreenCoordinateToWorldCoodinate (screenCoordinate, rasterPlane)
        Assert.Equal(-1.33, conversion.x, 2)
        Assert.Equal(1.00, conversion.y, 2)
    [<Fact>]
    let ``can convert bottom right edge screen coordinate to world coordinate`` () =
        let screenCoordinate = {i = 640; j = 480}
        let rasterPlane = {screenWidth = 640; screenHeight = 480; fov = 90}
        let conversion = mapScreenCoordinateToWorldCoodinate (screenCoordinate, rasterPlane)
        Assert.Equal(1.34, conversion.x, 2)
        Assert.Equal(-1.00, conversion.y, 2)
    [<Fact>]
    let ``can convert middle screen coordinate to world coordinate`` () =
        let screenCoordinate = {i = 320; j = 240}
        let rasterPlane = {screenWidth = 640; screenHeight = 480; fov = 90}
        let conversion = mapScreenCoordinateToWorldCoodinate (screenCoordinate, rasterPlane)
        Assert.Equal(0.00, conversion.x, 2)
        Assert.Equal(0.00, conversion.y, 2)