namespace Illuminate

module Math = 

    // Division and multiplication operators which cast ints 
    // to floats for themselves.  '.' on the side(s) which 
    // need(s) to be cast.

    /// Floating point division given int and float args.
    let deg2rad angle =
        System.Math.PI * angle / 180.0;