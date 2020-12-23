namespace Illuminate.Framework

open FsAlg.Generic
open Illuminate.Framework.Types

module Math = 
    let calculateTransformationMatrix trans =
        let translationMatrix = 
            match trans.translate with
                | None -> identityMatrix
                | Some translate -> matrix[[1.; 0.; 0.; translate.x];
                                       [0.; 1.; 0.; translate.y];
                                       [0.; 0.; 1.; translate.z];
                                       [0.; 0.; 0.; 1.]]
        let scaleMatrix = 
            match trans.scale with
                | None -> identityMatrix
                | Some scale -> matrix[[scale.x; 0.; 0.; 0.];
                                       [0.; scale.y; 0.; 0.];
                                       [0.; 0.; scale.z; 0.];
                                       [0.; 0.; 0.; 1.]]

        let rotationXMatrix = 
            match trans.rotation with
                | None -> identityMatrix
                | Some rotation -> matrix[[1.; 0.; 0.; 0.];
                                       [0.; cos(rotation.xDegrees); -sin(rotation.xDegrees); 0.];
                                       [0.; sin(rotation.xDegrees); cos(rotation.xDegrees); 0.];
                                       [0.; 0.; 0.; 1.]]
        let rotationYMatrix = 
            match trans.rotation with
                | None -> identityMatrix
                | Some rotation -> matrix[[cos(rotation.yDegrees); 0.; sin(rotation.yDegrees); 0.];
                                       [0.; 1.; 0.; 0.];
                                       [-sin(rotation.yDegrees); 0.; cos(rotation.yDegrees); 0.];
                                       [0.; 0.; 0.; 1.]]
        let rotationZMatrix = 
            match trans.rotation with
                | None -> identityMatrix
                | Some rotation -> matrix[[cos(rotation.zDegrees); -sin(rotation.zDegrees); 0.; 0.];
                                       [sin(rotation.zDegrees); cos(rotation.zDegrees); 0.; 0.];
                                       [0.; 0.; 1.; 0.];
                                       [0.; 0.; 0.; 1.]]

        translationMatrix * scaleMatrix * rotationXMatrix * rotationYMatrix * rotationZMatrix

    //adds data needed for vector to be multiplied by 4x4 translation matrix
    //exteremely important that arr comes before [|1|]
    //https://www.euclideanspace.com/maths/geometry/affine/matrix4x4/index.htm
    let addFourthDimension v =
        let arr = Vector.toArray v
        let arrWith1 = Array.append arr [|1.|]
        let vec = Vector.ofArray arrWith1
        vec

    //transform the vector by the translation matrix
    //have to convert vector to 4th dimension first
    let transformVector trans v = 
        let vWithFourth = addFourthDimension v
        let transformedMatrix = calculateTransformationMatrix(trans)
        let transformedV = transformedMatrix * vWithFourth
        transformedV.[..2]

    let calculateQ discr a b c = 
        let q =
            match b > 0. with
                | true -> -0.5 * (b + sqrt(discr))
                | false -> -0.5 * (b - sqrt(discr))
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