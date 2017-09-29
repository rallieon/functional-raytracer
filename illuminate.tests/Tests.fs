module Tests

open System
open Xunit
open FsAlg.Generic
open Illuminate.Types
open Illuminate.Core

[<Fact>]
let ``can convert bottom left edge screen coordinate to world coordinate`` () =
    let screenCoordinate = {x = 0; y = 0}
    let rasterPlane = {screenWidth = 100; screenHeight = 100;}
    let conversion = mapScreenCoordinateToWorldCoodinate screenCoordinate rasterPlane
    Assert.Equal(0., conversion.x)
    Assert.Equal(0., conversion.y)
[<Fact>]
let ``can convert top right edge screen coordinate to world coordinate`` () =
    let screenCoordinate = {x = 100; y = 100}
    let rasterPlane = {screenWidth = 100; screenHeight = 100;}
    let conversion = mapScreenCoordinateToWorldCoodinate screenCoordinate rasterPlane
    Assert.Equal(100., conversion.x)
    Assert.Equal(100., conversion.y)
[<Fact>]
let ``can convert middle screen coordinate to world coordinate`` () =
    let screenCoordinate = {x = 50; y = 50}
    let rasterPlane = {screenWidth = 100; screenHeight = 100;}
    let conversion = mapScreenCoordinateToWorldCoodinate screenCoordinate rasterPlane
    Assert.Equal(50., conversion.x)
    Assert.Equal(50., conversion.y)