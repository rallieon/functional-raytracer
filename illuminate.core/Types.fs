namespace Illuminate

open FsAlg.Generic

module Types = 
    (* Scene *)
    type WorldCoordinate = WorldCoordinate of Vector<float> //single discriminated union for type safety https://fsharpforfunandprofit.com/posts/designing-with-types-single-case-dus/
    type Direction = Direction of Vector<float>
    type Camera = WorldCoordinate
    type ViewPlane = { x: int; y:int }

    (* Glue *)
    type Ray = {direction: Direction; origin: WorldCoordinate}
    type Color = { r: float; g: float; b: float }

    (* Shapes *)
    type Sphere = {origin: WorldCoordinate; radius: float}

    (* Lighting *)
    type PointLight = {origin: WorldCoordinate; luminosity: Color}
    type SpotLight = {origin: WorldCoordinate; luminosity: Color; direction: Direction}
    type Light = PointLight | SpotLight

