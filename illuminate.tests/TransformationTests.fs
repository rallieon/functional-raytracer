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
        let transformedVector = (transformVector transformation vec).ToSeq()
        let expected = (vector[3.0;2.0;-4.0]).ToSeq()

        Assert.Equal<seq<float>>(expected, transformedVector)

    [<Fact>]
    let ``can scale a vector`` () =
        let vec = vector [2.0; 2.0; -4.0]
        let transformation = {translate=None; scale=Some {x=2.;y=1.;z=1.}; rotation=None}
        let transformedVector = (transformVector transformation vec).ToSeq()
        let expected = (vector[4.0;2.0;-4.0]).ToSeq()

        Assert.Equal<seq<float>>(expected, transformedVector)


    [<Fact>]
    let ``can scale a vector by multiple dimensions`` () =
        let vec = vector [2.0; 2.0; -4.0]
        let transformation = {translate=None; scale=Some {x=2.;y=2.;z=1.}; rotation=None}
        let transformedVector = (transformVector transformation vec).ToSeq()
        let expected = (vector[4.0;4.0;-4.0]).ToSeq()

        Assert.Equal<seq<float>>(expected, transformedVector)
    