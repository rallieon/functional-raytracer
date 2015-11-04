open System.Windows.Forms
open System.Drawing
open System.Windows.Media.Media3D

type Sphere = 
    { center : Point3D
      radius : float }

type Camera = 
    { position : Point3D
      lookAt : Point3D
      lookUp : Vector3D }

type Scene = 
    { camera : Camera
      sphere : Sphere }

type Ray = 
    { origin : Point3D
      direction : Vector3D }

let norm (v : Vector3D) = 
    let abs = sqrt (v.X * v.X + v.Y * v.Y + v.Z * v.Z)
    v / abs

let castRay ray scene = 
    let s = ray.origin - scene.sphere.center
    let rayDir = norm ray.direction
    let sv = Vector3D.DotProduct(s, rayDir)
    let ss = Vector3D.DotProduct(s, s)
    let discr = sv * sv - ss + scene.sphere.radius * scene.sphere.radius
    if discr < 0.0 then false
    else true

let buildScene = 
    do 
       let width = 480
       let height = 320
       // Vertical and horizontal field of view
       let hfov = System.Math.PI / 3.5
       let vfov = hfov * float (height) / float (width)
       // Pixel width and height
       let pw = 2.0 * System.Math.Tan(float (hfov / 2.0)) / float (width)
       let ph = 2.0 * System.Math.Tan(float (vfov / 2.0)) / float (height)
       let box = 
           new PictureBox(BackColor = Color.White, Dock = DockStyle.Fill, SizeMode = PictureBoxSizeMode.CenterImage)
       let bmp = new Bitmap(width, height)
       
       // sphere
       let sphere = 
           { center = Point3D(1.0, 1.0, 1.0)
             radius = 0.4 }
       
       // camera
       let camera = 
           { position = Point3D(0.0, 0.0, 0.0)
             lookAt = Point3D(1.0, 1.0, 1.0)
             lookUp = Vector3D(0.0, 1.0, 0.0) }
       
       // scene
       let scene = 
           { camera = camera
             sphere = sphere }
       
       // set up the coordinate system
       let n = norm (camera.position - camera.lookAt)
       let u = norm (Vector3D.CrossProduct(camera.lookUp, n))
       let v = norm (Vector3D.CrossProduct(n, u))
       let vpc = camera.position - n
       for x in 0..(width - 1) do
           for y in 0..(height - 1) do
               let rayPoint = vpc + float (x - width / 2) * pw * u + float (y - height / 2) * ph * v
               let rayDir = norm (rayPoint - scene.camera.position)
               
               let ray = 
                   { origin = scene.camera.position
                     direction = rayDir }
               match castRay ray scene with
               | true -> bmp.SetPixel(x, y, Color.Red)
               | false -> bmp.SetPixel(x, y, Color.Gray)
       bmp.Save("output.jpg")
       let path = System.Reflection.Assembly.GetExecutingAssembly().Location
       let directory = System.IO.Path.GetDirectoryName path

       ignore (System.Diagnostics.Process.Start(directory + "\\output.jpg"))

[<EntryPoint>]
let main argv = buildScene; 0 // return an integer exit code
