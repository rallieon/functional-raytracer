namespace Illuminate

open Xunit
open Illuminate.Framework.Math
open Illuminate.Framework.Types
open Illuminate.Framework.Coordinate

module MathTests = 

    [<Fact>]
    let ``can convert degrees to radians`` () =
        let angle = 20.
        let convert = deg2rad angle
        Assert.Equal(0.35, convert, 2)
        
    [<Fact>]
    let ``can perform dot product`` () =
        let vec1 = {x = 2.0; y = 3.0; z = 4.0}
        let vec2 = {x = 4.0; y = 5.0; z = 6.0}

        let result = dot vec1 vec2
        Assert.Equal(47.0, result, 2)
    
    [<Fact>]
    let ``can normalize world coordinate`` () =
        let normalized = normalizeWorld ({x = 2.0; y = 2.0; z = -5.0}, {i = 320; j = 240})
        Assert.Equal(0.3482, (fst normalized).dirX, 4)
        Assert.Equal(0.3482, (fst normalized).dirY, 4)
        Assert.Equal(-0.8704, (fst normalized).dirZ, 4)
        Assert.Equal(320, (snd normalized).i)
        Assert.Equal(240, (snd normalized).j)
    
    [<Fact>]
    let ``can subtract world coordinates`` () =
        let v = worldSubWorld {x = 2.0; y = 2.0; z = -5.0} {x = 0.0; y = 0.0; z = 0.0}
        Assert.Equal(2.0, v.x)
        Assert.Equal(2.0, v.y)
        Assert.Equal(-5.0, v.z)