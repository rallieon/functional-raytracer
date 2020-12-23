namespace Illuminate

open Xunit
open FsAlg.Generic
open Illuminate.Framework.Types
open Illuminate.Framework.Math

module TransformationTests = 
    [<Fact>]
    let ``can translate a vector`` () =
        let vec = vector [2.0; 2.0; -4.0]
        let transformation = {translate=Some {x=1.;y=0.;z=0.}; scale=None; rotation=None}
        let transformedVector = (transformVector transformation vec).ToArray()
        let expected = (vector[3.0;2.0;-4.0]).ToArray()

        Assert.Equal(expected.[0], transformedVector.[0])
        Assert.Equal(expected.[1], transformedVector.[1])
        Assert.Equal(expected.[2], transformedVector.[2])

    [<Fact>]
    let ``can scale a vector`` () =
        let vec = vector [2.0; 2.0; -4.0]
        let transformation = {translate=None; scale=Some {x=2.;y=1.;z=1.}; rotation=None}
        let transformedVector = (transformVector transformation vec).ToArray()
        let expected = (vector[4.0;2.0;-4.0]).ToArray()

        Assert.Equal(expected.[0], transformedVector.[0])
        Assert.Equal(expected.[1], transformedVector.[1])
        Assert.Equal(expected.[2], transformedVector.[2])


    [<Fact>]
    let ``can scale a vector by multiple dimensions`` () =
        let vec = vector [2.0; 2.0; -4.0]
        let transformation = {translate=None; scale=Some {x=2.;y=2.;z=1.}; rotation=None}
        let transformedVector = (transformVector transformation vec).ToArray()
        let expected = (vector[4.0;4.0;-4.0]).ToArray()

        Assert.Equal(expected.[0], transformedVector.[0])
        Assert.Equal(expected.[1], transformedVector.[1])
        Assert.Equal(expected.[2], transformedVector.[2])
    