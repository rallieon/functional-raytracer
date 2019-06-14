namespace Illuminate
open Illuminate.MathTypes

module Types = 
    (* Framework *)
    type WorldCoordinate = {x: float; y: float; z: float}
    type Direction =  {dirX: float; dirY: float; dirZ: float}
    type Camera = WorldCoordinate
    type Ray = {direction: Direction; origin: WorldCoordinate}
    type Color = { r: float; g: float; b: float }
    type ScreenCoordinate = { i: int; j: int; }
    type Pixel = { coordinate: ScreenCoordinate; pixelColor: Color }
    type Image = Pixel list

    (* Shapes *)
    type Sphere = {origin: WorldCoordinate; radius: float; color: Color}
    type Plane = {origin: WorldCoordinate; width: float; length: float; color: Color}
    type Shape = 
        | Sphere of Sphere
        | Plane of Plane

    type HitPoint = {shape: Shape; t: float; point: WorldCoordinate; normal: Normal; shadowOrigin: WorldCoordinate}

    (* Lighting *)
    type PointLight = {origin: WorldCoordinate; luminosity: Color; intensity: float}
    type SpotLight = {origin: WorldCoordinate; luminosity: Color; intensity: float; direction: Direction}
    type Light = 
        | PointLight of PointLight 
        | SpotLight of SpotLight

    type LightHitPoint = {lightDistance: float; lightDirection: Direction; lightHit: HitPoint option; light: Light}
    (* Scene *)
    type Scene = { width: int; height: int; fov: int; shapes: Shape list; lights: Light list; camera: Camera}