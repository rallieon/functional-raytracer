namespace Illuminate

open Xunit
open Illuminate.Types
open Illuminate.Render
open Illuminate.Image
open Illuminate.SceneReader

module IntegrationTests = 
    let getScene =
        readScene "../../../../scenes/integrationtest.json"

    [<Fact>]
    let ``can render an image of appropriate size`` () =
        let scene = getScene
        let t = render scene
        Assert.Equal(307200, t.Length)