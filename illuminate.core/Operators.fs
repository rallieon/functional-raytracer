namespace Illuminate

module Operators = 

    // Division and multiplication operators which cast ints 
    // to floats for themselves.  '.' on the side(s) which 
    // need(s) to be cast.

    /// Floating point division given int and float args.
    let (./) x y = 
        (x |> float) / y

    /// Floating point division given float and int args.
    let (/.) x y = 
        x / (y |> float)

    /// Floating point division given int and int args.
    let (./.) x y = 
        (x |> float) / (y |> float)

    /// Floating point multiplication given int and float args.
    let (.*) x y = 
        (x |> float) * y

    /// Floating point multiplication given float and int args.
    let ( *. ) x y = 
        x * (y |> float)

    /// Floating point multiplication given int and int args.
    let (.*.) x y = 
        (x |> float) * (y |> float)