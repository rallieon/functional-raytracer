namespace Illuminate

open Xunit
open Illuminate.Math
open Illuminate.Types
open Illuminate.Coordinate

module MathTests = 

    [<Fact>]
    let ``can convert degrees to radians`` () =
        let angle = 20.
        let convert = deg2rad angle
        Assert.Equal(0.35, convert, 2)
    [<Fact>]
    let ``can perform dot product`` () =
        let vec1 = (2.0,3.0,4.0)
        let vec2 = (4.0,5.0,6.0)

        let result = dotProduct (vec1, vec2)
        Assert.Equal(47.0, result, 2)