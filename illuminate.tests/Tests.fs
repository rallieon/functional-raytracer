module Tests

open System
open Xunit
open Illuminate.Math

[<Fact>]
let ``My test`` () =
    Assert.True(true)
[<Fact>]
let ``test add`` () =
    let x = add 1 2
    Assert.Equal(3, x)

[<Fact>]
let ``test sub`` () =
    let x = sub 1 2
    Assert.Equal(-1, x)