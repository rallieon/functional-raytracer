namespace Illuminate

open Xunit
open FsAlg.Generic
open Illuminate.Framework.Types
open Illuminate.Framework.Math
open System

module TransformationTests =
    [<Fact>]
    let ``can translate a vector`` () =
        let vec = vector [ 2.0; 2.0; -4.0 ]

        let transformations =
            [ Translation { x = 1.; y = 0.; z = 0. } ]

        let transformedVector =
            (transformVector (transformations, vec)).ToSeq()

        let expected = (vector [ 3.0; 2.0; -4.0 ]).ToSeq()

        Assert.Equal<seq<float>>(expected, transformedVector)

    [<Fact>]
    let ``can scale a vector`` () =
        let vec = vector [ 2.0; 2.0; -4.0 ]

        let transformations = [ Scale { x = 2.; y = 1.; z = 1. } ]

        let transformedVector =
            (transformVector (transformations, vec)).ToSeq()

        let expected = (vector [ 4.0; 2.0; -4.0 ]).ToSeq()

        Assert.Equal<seq<float>>(expected, transformedVector)


    [<Fact>]
    let ``can scale a vector by multiple dimensions`` () =
        let vec = vector [ 2.0; 2.0; -4.0 ]

        let transformations = [ Scale { x = 2.; y = 2.; z = 1. } ]

        let transformedVector =
            (transformVector (transformations, vec)).ToSeq()

        let expected = (vector [ 4.0; 4.0; -4.0 ]).ToSeq()

        Assert.Equal<seq<float>>(expected, transformedVector)

    [<Fact>]
    let ``can rotate a vector around the x axis`` () =
        let vec = vector [ 2.0; 2.0; -4.0 ]

        let transformations = [ RotateX { radians = (Math.PI / 4.) } ]

        let transformedVector =
            (transformVector (transformations, vec)).ToSeq()

        let expected =
            (vector [ 2.
                      4.242640687119285
                      -1.4142135623730954 ])
                .ToSeq()

        Assert.Equal<seq<float>>(expected, transformedVector)

    [<Fact>]
    let ``can rotate a vector around the y axis`` () =
        let vec = vector [ 2.0; 2.0; -4.0 ]

        let transformations = [ RotateY { radians = (Math.PI / 4.) } ]

        let transformedVector =
            (transformVector (transformations, vec)).ToSeq()

        let expected =
            (vector [ -1.4142135623730947
                      2.
                      -4.242640687119286 ])
                .ToSeq()

        Assert.Equal<seq<float>>(expected, transformedVector)

    [<Fact>]
    let ``can rotate a vector around the z axis`` () =
        let vec = vector [ 2.0; 2.0; -4.0 ]

        let transformations = [ RotateZ { radians = (Math.PI / 4.) } ]

        let transformedVector =
            (transformVector (transformations, vec)).ToSeq()

        let expected =
            (vector [ 2.220446049250313E-16
                      2.82842712474619
                      -4. ])
                .ToSeq()

        Assert.Equal<seq<float>>(expected, transformedVector)

    [<Fact>]
    let ``can perform an empty rotation around all axis`` () =
        let vec = vector [ 2.0; 2.0; -4.0 ]

        let transformations =
            [ RotateX { radians = 0. }
              RotateY { radians = 0. }
              RotateZ { radians = 0. } ]

        let transformedVector =
            (transformVector (transformations, vec)).ToSeq()

        let expected = (vector [ 2.; 2.; -4. ]).ToSeq()

        Assert.Equal<seq<float>>(expected, transformedVector)

    [<Fact>]
    let ``can perform a rotation around all axis`` () =
        let vec = vector [ 2.0; 2.0; -4.0 ]

        let transformations =
            [ RotateX { radians = (Math.PI / 4.) }
              RotateY { radians = (Math.PI / 4.) }
              RotateZ { radians = (Math.PI / 4.) } ]

        let transformedVector =
            (transformVector (transformations, vec)).ToSeq()

        let expected =
            (vector [ -2.707106781186547
                      3.2928932188134525
                      -2.4142135623730954 ])
                .ToSeq()

        Assert.Equal<seq<float>>(expected, transformedVector)

    [<Fact>]
    let ``can perform a rotation and translation`` () =
        let vec = vector [ 3.; 2.; 1. ]

        let transformations =
            [ Translation { x = -2.; y = -2.; z = -2. }
              RotateX { radians = (Math.PI / 3.) } ]

        let transformedVector =
            (transformVector (transformations, vec)).ToSeq()

        let expected =
            (vector [ 1.
                      0.8660254037844386
                      -0.5000000000000004 ])
                .ToSeq()

        Assert.Equal<seq<float>>(expected, transformedVector)
