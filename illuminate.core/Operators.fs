namespace Illuminate

module Operators = 
    let (./) x y = 
        (x |> float) / y
    let (/.) x y = 
        x / (y |> float)
    let (./.) x y = 
        (x |> float) / (y |> float)
    let (.*) x y = 
        (x |> float) * y

    let ( *. ) x y = 
        x * (y |> float)
    let (.*.) x y = 
        (x |> float) * (y |> float)
    
    let (.+) x y = 
        (x |> float) + y
    let (+.) x y = 
        x + (y |> float)
    let (.+.) x y = 
        (x |> float) + (y |> float)
    let (.-) x y = 
        (x |> float) - y
    let (-.) x y = 
        x - (y |> float)
    let (.-.) x y = 
        (x |> float) - (y |> float)