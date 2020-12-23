namespace Illuminate.Framework

open FsAlg.Generic
open Illuminate.Framework.Types

module Math =
    let createTransform (transform: Transformation): TransformationMatrix =
        match transform with
        | Translation trans ->
            matrix [ [ 1.; 0.; 0.; trans.x ]
                     [ 0.; 1.; 0.; trans.y ]
                     [ 0.; 0.; 1.; trans.z ]
                     [ 0.; 0.; 0.; 1. ] ]

        | Scale trans ->
            matrix [ [ trans.x; 0.; 0.; 0. ]
                     [ 0.; trans.y; 0.; 0. ]
                     [ 0.; 0.; trans.z; 0. ]
                     [ 0.; 0.; 0.; 1. ] ]

        | RotateX trans ->
            matrix [ [ 1.; 0.; 0.; 0. ]
                     [ 0.
                       cos (trans.radians)
                       -sin (trans.radians)
                       0. ]
                     [ 0.
                       sin (trans.radians)
                       cos (trans.radians)
                       0. ]
                     [ 0.; 0.; 0.; 1. ] ]
        | RotateY trans ->
            matrix [ [ cos (trans.radians)
                       0.
                       sin (trans.radians)
                       0. ]
                     [ 0.; 1.; 0.; 0. ]
                     [ -sin (trans.radians)
                       0.
                       cos (trans.radians)
                       0. ]
                     [ 0.; 0.; 0.; 1. ] ]

        | RotateZ trans ->
            matrix [ [ cos (trans.radians)
                       -sin (trans.radians)
                       0.
                       0. ]
                     [ sin (trans.radians)
                       cos (trans.radians)
                       0.
                       0. ]
                     [ 0.; 0.; 1.; 0. ]
                     [ 0.; 0.; 0.; 1. ] ]

    let createTransformationMatrix (transformations: Transformations): TransformationMatrix =
        //have to reverse the list to make the matrix math work in the right order.
        transformations
        |> List.rev
        |> List.fold (fun acc elem -> acc * createTransform (elem)) identityMatrix

    //adds data needed for vector to be multiplied by 4x4 translation matrix
    //exteremely important that arr comes before [|1|]
    //https://www.euclideanspace.com/maths/geometry/affine/matrix4x4/index.htm
    let addFourthDimension v =
        let arr = Vector.toArray v
        let arrWith1 = Array.append arr [| 1. |]
        let vec = Vector.ofArray arrWith1
        vec

    //transform the vector by the translation matrix
    //have to convert vector to 4th dimension first
    let transformVector (transformations: Transformations, v: Vector<float>): Vector<float> =
        let vWithFourth = addFourthDimension v

        let transformedMatrix =
            createTransformationMatrix transformations

        let transformedV = transformedMatrix * vWithFourth
        transformedV.[..2]

    let calculateQ discr a b c =
        let q =
            match b > 0. with
            | true -> -0.5 * (b + sqrt (discr))
            | false -> -0.5 * (b - sqrt (discr))

        let x0 = q / a
        let x1 = c / q

        let t0 =
            match x0 > x1 with
            | true -> x1
            | false -> x0

        let t1 =
            match x0 > x1 with
            | true -> x0
            | false -> x1

        (true, t0, t1)

    let solveQuadratic a b c =
        let discr = b * b - 4. * a * c

        match discr < 0., discr = 0. with
        | true, _ -> (false, 0., 0.)
        | false, true -> (true, -0.5 * b / a, -0.5 * b / a)
        | false, false -> calculateQ discr a b c
