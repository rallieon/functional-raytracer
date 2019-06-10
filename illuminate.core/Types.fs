namespace Illuminate

module Types = 
    (* Framework *)
    type WorldCoordinate = {x: float; y: float; z: float}
    type Direction =  {dirX: float; dirY: float; dirZ: float}
    type Camera = WorldCoordinate
    type ViewPlane = { screenWidth: int; screenHeight: int ; fov: int}
    type Ray = {direction: Direction; origin: WorldCoordinate}
    type Color = { r: int; g: int; b: int }
    type ScreenCoordinate = { i: int; j: int; }
    type Pixel = { coordinate: ScreenCoordinate; color: Color }
    type Image = Pixel list

    (* Shapes *)
    type Sphere = {origin: WorldCoordinate; radius: float; color: Color}
    type Plane = {origin: WorldCoordinate; width: float; length: float; color: Color}
    type Shape = 
        | Sphere of Sphere
        | Plane of Plane

    type HitPoint = Shape * float

    (* Lighting *)
    type PointLight = {origin: WorldCoordinate; luminosity: Color}
    type SpotLight = {origin: WorldCoordinate; luminosity: Color; direction: Direction}
    type Light = 
        | PointLight of PointLight 
        | SpotLight of SpotLight

    (* Scene *)
    type Scene = { width: int; height: int; fov: int; shapes: Shape list; lights: Light list; camera: Camera}
