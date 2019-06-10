namespace Illuminate

open Illuminate.MathTypes

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