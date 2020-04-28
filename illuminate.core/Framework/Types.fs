namespace Illuminate.Framework
open FsAlg.Generic
open FSharp.Json

module Types = 
    (* Framework *)
    type WorldCoordinate = Vector<float>
    type Direction = Vector<float>
    type Camera = { cameraOrigin: WorldCoordinate }
    type Normal = Vector<float>
    type Ray = {direction: Direction; origin: WorldCoordinate}
    type Color = { r: float; g: float; b: float }
    type ScreenCoordinate = { i: int; j: int; }
    type Pixel = { coordinate: ScreenCoordinate; pixelColor: Color }
    type Image = Pixel list
    
    (*Transformations*)
    type TranslationTransformation = {x: float; y: float; z: float}
    type ScaleTransformation = {x: float; y: float; z: float}
    type RotationTransformation = {xDegrees: float; yDegrees: float; zDegrees: float}
    type Transformation = {translate: TranslationTransformation option; 
        scale: ScaleTransformation option; rotation: RotationTransformation option}

    (* Shapes *)
    type Sphere = {origin: WorldCoordinate; radius: float; color: Color;
        transformation: Transformation option}
    type Plane = {planePoint: WorldCoordinate; planeNormal: Normal; color: Color;
        transformation: Transformation option}
    type Triangle = {v0: WorldCoordinate; v1: WorldCoordinate; v2: WorldCoordinate; color: Color; 
        triangleNormal: WorldCoordinate option; transformation: Transformation option}
    type Box = {vMin: WorldCoordinate; vMax: WorldCoordinate; color:  Color; 
        transformation: Transformation option}
    type TriangleMesh = {filePath: string; color:  Color; triangles: Triangle list; 
        transformation: Transformation option}

    [<ReferenceEquality>]
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

    let identityMatrix = matrix [[1.; 0.; 0.; 0.]; [0.; 1.; 0.; 0.]; [0.; 0.; 1.; 0.]; [0.; 0.; 0.; 1.]]

    let debug pixel scene =
        let checkVal i j =
            match scene.debug, i = pixel.i, j = pixel.j with
            | true, true, true -> printf ""
            | _ -> ()

        match scene.debugi, scene.debugj with 
            | Some i, Some j -> checkVal i j
            | None, _ -> ()
            | Some i, _ -> ()