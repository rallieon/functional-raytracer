namespace Illuminate

open Xunit
open Illuminate.Framework.Render
open Illuminate.Framework.Types
open Illuminate.Framework.Coordinate
open FsAlg.Generic

module MathTests = 
    [<Fact>]
    let ``can perform dot product`` () =
        let vec1 = vector [2.0; 3.0; 4.0]
        let vec2 = vector [4.0; 5.0; 6.0]

        let result = vec1 * vec2
        Assert.Equal(47.0, result, 2)
    
    [<Fact>]
    let ``can normalize world coordinate`` () =
        let normalized = normalizeWorld (vector [2.0; 2.0; -5.0], {i = 320; j = 240})
        Assert.Equal(0.3482, (fst normalized).[0], 4)
        Assert.Equal(0.3482, (fst normalized).[1], 4)
        Assert.Equal(-0.8704, (fst normalized).[2], 4)
        Assert.Equal(320, (snd normalized).i)
        Assert.Equal(240, (snd normalized).j)
    
    [<Fact>]
    let ``can subtract world coordinates`` () =
        let v = vector [2.0; 2.0; -5.0] - vector [0.0; 0.0; 0.0]
        Assert.Equal(2.0, v.[0])
        Assert.Equal(2.0, v.[1])
        Assert.Equal(-5.0, v.[2])