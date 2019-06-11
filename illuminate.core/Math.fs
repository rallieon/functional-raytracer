namespace Illuminate

open Illuminate.MathTypes
open Illuminate.Types

module Math = 

    // Division and multiplication operators which cast ints 
    // to floats for themselves.  '.' on the side(s) which 
    // need(s) to be cast.

    /// Floating point division given int and float args.
    let deg2rad angle =
        System.Math.PI * angle / 180.0;
    
    let dotProduct (vector:Vector, vector2:Vector) = 
        let x,y,z = vector
        let x2, y2, z2 = vector2
        x * x2 + y * y2 + z * z2
    
    let inverseMag (mag2:float, v:Vector) : Normal = 
        let x,y,z = v
        let invMag = 1. / sqrt mag2
        (x * invMag, y * invMag, z * invMag)

    let normalizeVector (v:Vector) : Normal = 
        let x,y,z = v
        let mag2 = x * x + y * y + z * z
        match mag2 > 0. with
            | true -> inverseMag (mag2, v)
            | false -> (x, y, z)
    
    let convertNormalToDirection (normal:Normal) : Direction =
        let x,y,z = normal
        {dirX = x; dirY = y; dirZ = z}
    
    let convertDirectionToNormal (direction:Direction) : Normal =
        (direction.dirX, direction.dirY, direction.dirZ)

    let convertDirectionToVector (direction:Direction) : Vector =
        (direction.dirX, direction.dirY, direction.dirZ)
    
    let normalizeDirection (v:Direction) : Normal = 
        convertDirectionToVector(v) |> normalizeVector
    
    let getDirectionFromVector vector =
        let x,y,z = vector
        {dirX = x; dirY = y; dirZ = z}