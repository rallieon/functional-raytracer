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
    let sceneSpec = SceneSpecification.Parse("./SceneExample.json")

    // Make ourselves a canvas
    let width = sceneSpec.Width
    let height = sceneSpec.Height

    // Vertical and horizontal field of view
    let hfov = System.Math.PI/3.2
    let vfov = hfov * float(height)/float(width)

    // Pixel width and height
    let pw = 2.0 * System.Math.Tan(float(hfov/2.0))/float(width)
    let ph = 2.0 * System.Math.Tan(float(vfov/2.0))/float(height)

    
    ignore 0
    // scene
    (*let scene = { camera=camera; lighting=lighting; shapes = [sphere1;] }

    // set up the coordinate system
    let n = norm (scene.camera.position - scene.camera.lookAt)
    let u = norm (scene.camera.lookUp.CrossProduct(n))
    let v = norm (n.CrossProduct(u))
    let vpc = scene.camera.position - n

    // Setting up the UI components
    let mainForm = new Form(Width = width, Height = height, Text = "FRay")
    let box = new PictureBox(BackColor = Color.White, Dock = DockStyle.Fill, SizeMode = PictureBoxSizeMode.CenterImage)
    mainForm.Controls.Add(box)
    let bmp = new Bitmap(width,height)

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
    Application.Run(mainForm)*)