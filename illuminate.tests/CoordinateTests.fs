namespace Illuminate

open Xunit
open Illuminate.Math
open Illuminate.Types
open Illuminate.Coordinate

module CoordinateTests = 
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
    
    [<Fact>]
    let ``can calculate screen coordinate from index 0`` () =
        let screenCoordinate = calculateScreenCoordinateFromIndex 0 640
        Assert.Equal(0, screenCoordinate.i)
        Assert.Equal(0, screenCoordinate.j)
    
    [<Fact>]
    let ``can calculate screen coordinate from index lower than width`` () =
        let screenCoordinate = calculateScreenCoordinateFromIndex 320 640
        Assert.Equal(320, screenCoordinate.i)
        Assert.Equal(0, screenCoordinate.j)
    
    [<Fact>]
    let ``can calculate screen coordinate from index higher than width`` () =
        let screenCoordinate = calculateScreenCoordinateFromIndex 750 640
        Assert.Equal(110, screenCoordinate.i)
        Assert.Equal(1, screenCoordinate.j)