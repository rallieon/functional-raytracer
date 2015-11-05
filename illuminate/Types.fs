module Types

type Color(r: float, g: float, b:float) =
    member this.r = r
    member this.g = g
    member this.b = b
    static member ( * ) (c1:Color, c2:Color) =
        Color (c1.r*c2.r, c1.g*c2.g, c1.b*c2.b)
    static member ( * ) (c:Color, s:float) =
        Color (c.r*s, c.g*s, c.b*s)
    static member ( + ) (c1:Color, c2:Color) = // there must be a less ugly way to do this
        let (r,g,b) = (c1.r + c2.r, c1.g + c2.g, c1.b + c2.b)
        let r = match c1.r+c2.r with
                |_ when c1.r+c2.r > 1.0 -> 1.0
                |_ -> c1.r+c2.r
        let g = match c1.g+c2.g with
                |_ when c1.g+c2.g > 1.0 -> 1.0
                |_ -> c1.g+c2.g
        let b = match c1.b+c2.b with
                |_ when c1.b+c2.b > 1.0 -> 1.0
                |_ -> c1.b+c2.b
        Color (r,g,b)
    static member Zero = Color(0.0,0.0,0.0)

type Vector3D(x: float, y: float, z: float) = 
    member this.x = x
    member this.y = y
    member this.z = z
    static member ( * ) (c1:Vector3D, c2:Vector3D) =
        Vector3D (c1.x*c2.x, c1.y*c2.y, c1.z*c2.z)
    static member ( * ) (c:Vector3D, s:float) =
        Vector3D (c.x*s, c.y*s, c.z*s)
    static member ( / ) (c:Vector3D, s:float) =
        Vector3D (c.x/s, c.y/s, c.z/s)
    static member ( + ) (c:Vector3D, s:float) =
        Vector3D (c.x+s, c.y+s, c.z+s)
    static member ( + ) (c1:Vector3D, c2:Vector3D) = // there must be a less ugly way to do this
        let (x,y,z) = (c1.x + c2.x, c1.y + c2.y, c1.z + c2.z)
        Vector3D (x,y,z)
    static member ( - ) (c1:Vector3D, c2:Vector3D) = // there must be a less ugly way to do this
        let (x,y,z) = (c1.x - c2.x, c1.y - c2.y, c1.z - c2.z)
        Vector3D (x,y,z)
    member this.DotProduct(c2: Vector3D) = 
        this.x * c2.x + this.y * c2.y + this.z * c2.z
    member this.CrossProduct(c2: Vector3D) = 
        let (x,y,z) = (this.y*c2.z - this.z*c2.y, this.z*c2.x - this.x*c2.z, this.x*c2.y - this.y*c2.x)
        Vector3D(x,y,z)
    static member Zero = Vector3D(0.0,0.0,0.0)

type Point3D(x: float, y: float, z: float) = 
    member this.x = x
    member this.y = y
    member this.z = z
    static member ( * ) (c1:Point3D, c2:Point3D) =
        Point3D (c1.x*c2.x, c1.y*c2.y, c1.z*c2.z)
    static member ( * ) (c:Point3D, s:float) =
        Point3D (c.x*s, c.y*s, c.z*s)
    static member ( / ) (c:Point3D, s:float) =
        Point3D (c.x/s, c.y/s, c.z/s)
    static member ( + ) (c:Point3D, s:float) =
        Point3D (c.x+s, c.y+s, c.z+s)
    static member ( + ) (c:Point3D, s:Vector3D) =
        Point3D (c.x+s.x, c.y+s.y, c.z+s.z)
    static member ( + ) (c1:Point3D, c2:Point3D) = // there must be a less ugly way to do this
        let (x,y,z) = (c1.x + c2.x, c1.y + c2.y, c1.z + c2.z)
        Point3D (x,y,z)
    static member ( - ) (c1:Point3D, c2:Point3D) = // there must be a less ugly way to do this
        let (x,y,z) = (c1.x - c2.x, c1.y - c2.y, c1.z - c2.z)
        Vector3D (x,y,z)
    static member ( - ) (c1:Point3D, c2:Vector3D) = // there must be a less ugly way to do this
        let (x,y,z) = (c1.x - c2.x, c1.y - c2.y, c1.z - c2.z)
        Point3D (x,y,z)
    static member Zero = Point3D(0.0,0.0,0.0)

type Time     = float
type Center   = Point3D
type Radius   = float
type Normal   = Vector3D

type Light    = { position:Point3D; color:Color }
type Lighting = { ambientLight:Color; lights:list<Light> }

type Camera   = { position:Point3D; lookAt:Point3D; lookUp:Vector3D }

type Material = { reflectivity:float; diffuseColor:Color; specularColor:Color; shininess:float}

type Ray      = { origin:Point3D; direction:Vector3D }
type Intersection = { normal:Normal; point:Point3D; ray:Ray; material:Material }

type Shape =
    | Sphere     of Center * Radius  * (Point3D -> Material)
    | Plane      of Normal * Point3D * (Point3D -> Material)
    | BoundPlane of Normal * Point3D * Point3D * (Point3D -> Material)

type Scene    = { camera:Camera; lighting:Lighting; shapes:list<Shape> }

// Epsilon is used for comparing equality with floating point numbers to avoid rounding errors
let Epsilon = 0.0001
// Background color
let Background = Color(0.0,0.0,0.0)
// FadeDistance determines the distance at which rays fade to background color
let FadeDistance = 30.0
// MaxReflections determines the number of times a light ray can bounce before we kill it. This
// prevents infinite relfections
let MaxReflections = 10