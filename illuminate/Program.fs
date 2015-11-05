module Program

open System
open System.Drawing
open System.Windows.Forms
open Types
open Core
open Material
open Color
open Shape
open Ray
open Scene
open Light
open Camera

[<STAThread>]
do
    // Make ourselves a canvas
    let width = 640
    let height = 480

    // Vertical and horizontal field of view
    let hfov = System.Math.PI/3.2
    let vfov = hfov * float(height)/float(width)

    // Pixel width and height
    let pw = 2.0 * System.Math.Tan(float(hfov/2.0))/float(width)
    let ph = 2.0 * System.Math.Tan(float(vfov/2.0))/float(height)    

    // Setting up the UI components
    let mainForm = new Form(Width = width, Height = height, Text = "FRay")
    let box = new PictureBox(BackColor = Color.White, Dock = DockStyle.Fill, SizeMode = PictureBoxSizeMode.CenterImage)
    mainForm.Controls.Add(box)
    let bmp = new Bitmap(width,height)

    // Some colors, including one given as a higher order function
    let color1 = Color(0.31,0.43,1.0)
    let color2 = Color(1.0,0.416,0.0)
    let lightColor = Color(0.5,0.5,0.5)
    let checkerFunc (p:Point3D) size =
        let (x,y,z) = (p.x, p.y, p.z)
        let jump x y z =
            let j = int((int(Math.Abs(float(x))/size) + int(Math.Abs(float(y))/size) + int(Math.Abs(float(z))/size)) % 2)
            match Math.Abs(j) with
            |_ when j = 1 -> Color(0.0,0.0,0.0)
            |_ -> Color(1.0,1.0,1.0)
        jump x y z

    // Some materials
    let material1 = { reflectivity=0.2; diffuseColor=color1; specularColor = Color(1.0,1.0,1.0); shininess=500.0 }
    let material2 = { reflectivity=0.2; diffuseColor=color2; specularColor = Color(1.0,1.0,1.0); shininess=500.0 }

    // Spheres
    let sphere0 = Sphere(Point3D(0.0,0.0,0.0), 0.7, (fun x -> material1))
    //let sphere1 = Sphere(Point3D(0.0,-0.7,0.0), 0.5, (fun x -> material1))
    let sphere1 = Sphere(Point3D(1.5,0.2,1.5), 0.5, (fun x -> material2))
    let sphere2 = Sphere(Point3D(-1.5,0.2,1.5), 0.5, (fun x -> material2))
    let sphere3 = Sphere(Point3D(1.5,0.2,-1.5), 0.5, (fun x -> material2))
    let sphere4 = Sphere(Point3D(-1.5,0.2,-1.5), 0.5, (fun x -> material2))
    
    // Floor plane
    let floor   = Plane(Vector3D(0.0,-1.0,0.0), Point3D(0.0,1.0,0.0), (fun x -> { reflectivity = 0.2; diffuseColor = checkerFunc x 1.5; specularColor = Color(1.0,1.0,1.0); shininess=500.0 }))

    // Janky way of constructing a box
    let top   = BoundPlane(Vector3D(0.0,-1.0,0.0), Point3D(-2.0,0.7,-2.0), Point3D(2.0,-0.7,2.0), (fun x -> material1))
    let side1 = BoundPlane(Vector3D(1.0,0.0,0.0), Point3D(2.0,1.0,-2.0), Point3D(2.0,0.7,2.0),    (fun x -> material1))
    let side2 = BoundPlane(Vector3D(-1.0,0.0,0.0), Point3D(-2.0,1.0,-2.0), Point3D(-2.0,0.7,2.0), (fun x -> material1))
    let side3 = BoundPlane(Vector3D(0.0,0.0,1.0), Point3D(-2.0,1.0,2.0), Point3D(2.0,0.7,2.0),    (fun x -> material1))
    let side4 = BoundPlane(Vector3D(0.0,0.0,-1.0), Point3D(-2.0,1.0,-2.0), Point3D(2.0,0.7,-2.0), (fun x -> material1))
    
    // lighting
    let light1 = { position = Point3D(5.0,-2.0,0.0); color = Color(0.9,0.9,0.9) }
    let light2 = { position = Point3D(-5.0,-2.0,0.0); color = Color(0.7,0.7,0.7) }
    let light3 = { position = Point3D(0.0,-2.0,5.0); color = Color(0.3,0.3,0.3) }
    let light4 = { position = Point3D(0.0,-2.0,-5.0); color = Color(0.2,0.2,0.2) }
    let lighting = { ambientLight=lightColor; lights=[light1;light2;light3;light4] }
    
    // camera
    let camera = { position=Point3D(1.0,-1.7,5.0); lookAt=Point3D(0.0,0.8,0.0); lookUp=Vector3D(0.0,1.0,0.0) }
    
    // scene
    let scene = { camera=camera; lighting=lighting; shapes = [sphere0;floor;top;side1;side2;side3;side4;sphere1;sphere2;sphere3;sphere4;] }

    // set up the coordinate system
    let n = norm (scene.camera.position - scene.camera.lookAt)
    let u = norm (scene.camera.lookUp.CrossProduct(n))
    let v = norm (n.CrossProduct(u))
    let vpc = scene.camera.position - n

    // render the scene
    let stopwatch = new System.Diagnostics.Stopwatch()
    Console.WriteLine("Building scene at resolution {0}*{1}...", width, height)
    stopwatch.Start()

    let image = Array.Parallel.init width  (fun x ->
                Array.init height (fun y -> 
                    let rayPoint = vpc + u*float(x-width/2)*pw + v*float(y-height/2)*ph
                    let rayDir = norm (rayPoint - scene.camera.position)
                    let ray = { origin = scene.camera.position; direction = rayDir }
                    let color = castRay ray scene 0
                    let (a,r,g,b) = argbFromColor(color)
                    Color.FromArgb(a,r,g,b)))

    stopwatch.Stop()
    Console.WriteLine("Time to render scene: {0:####}.{1:##} sec", stopwatch.Elapsed.Seconds, stopwatch.Elapsed.Milliseconds)
    stopwatch.Reset()

    // build the bitmap from the rendered scene
    stopwatch.Start()
    for x in 0..(image.Length-1) do
        for y in 0..(image.[x].Length-1) do 
            bmp.SetPixel(x,y,image.[x].[y])
    stopwatch.Stop();
    Console.WriteLine("Time to build bitmap: {0:####}.{1:##} sec", stopwatch.Elapsed.Seconds, stopwatch.Elapsed.Milliseconds)
    
    // save the bitmap to disk.
    bmp.Save("FRayOutput.jpg");

    // send the bitmap to our window to display
    box.Image <- (bmp :> Image)
    Application.Run(mainForm)