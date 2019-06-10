namespace Illuminate

open Xunit
open Illuminate.Math
open Illuminate.Types
open Illuminate.Core
open Illuminate.Coordinate

module MathTests = 

    [<Fact>]
    let ``can convert degrees to radians`` () =
        let angle = 20.
        let convert = deg2rad angle
        Assert.Equal(0.35, convert, 2)