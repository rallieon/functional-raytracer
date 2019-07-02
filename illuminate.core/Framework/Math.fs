namespace Illuminate.Framework

open Illuminate.Framework.Types

module Math = 
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