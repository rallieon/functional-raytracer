namespace Illuminate

module Types = 
    (* Framework *)
    type WorldCoordinate = {x: float; y: float; z: float}
    type Direction =  {dirX: float; dirY: float; dirZ: float}
    type Camera = WorldCoordinate
    type Vector = WorldCoordinate
    type Normal = WorldCoordinate
    type Ray = {direction: Direction; origin: WorldCoordinate}
    type Color = { r: float; g: float; b: float }
    type ScreenCoordinate = { i: int; j: int; }
    type Pixel = { coordinate: ScreenCoordinate; pixelColor: Color }
    type Image = Pixel list

    (* Shapes *)
    type Sphere = {origin: WorldCoordinate; radius: float; color: Color}
    type Plane = {planePoint: WorldCoordinate; planeNormal: Normal; color: Color}
    type Triangle = {v0: WorldCoordinate; v1: WorldCoordinate; v2: WorldCoordinate; color: Color}
    type Box = {vMin: WorldCoordinate; vMax: WorldCoordinate; color:  Color}
    type TriangleMesh = {filePath: string; color:  Color}
    type Shape = 
        | Sphere of Sphere
        | Plane of Plane
        | Triangle of Triangle
        | Box of Box
        | TriangleMesh of TriangleMesh

    type HitPoint = {shape: Shape; t: float; point: WorldCoordinate; normal: Normal; shadowOrigin: WorldCoordinate}

    (* Lighting *)
    type PointLight = {pointOrigin: WorldCoordinate; luminosity: Color; intensity: float}
    type DistantLight = {distantLightDirection: Direction; luminosity: Color; intensity: float}
    type SpotLight = {spotOrigin: WorldCoordinate; luminosity: Color; intensity: float; direction: Direction}
    type Light = 
        | PointLight of PointLight
        | DistantLight of DistantLight
        | SpotLight of SpotLight

    type LightHitPoint = {lightDistance: float; lightDirection: Direction; luminosity: Color}
    (* Scene *)
    type Scene = { width: int; height: int; fov: int; shapes: Shape list; lights: Light list; camera: Camera; debugi: int option; debugj: int option; debug: bool}

    [<Literal>]
    let infinity = System.Double.MaxValue

    [<Literal>]
    let epsilon = 0.000001

    [<Literal>]
    let bias = 0.0001

    let debug pixel scene =
        let checkVal i j =
            match scene.debug, i = pixel.i, j = pixel.j with
            | true, true, true -> printf ""
            | _ -> ()

        match scene.debugi, scene.debugj with 
            | Some i, Some j -> checkVal i j
            | None, _ -> ()
            | Some i, _ -> ()