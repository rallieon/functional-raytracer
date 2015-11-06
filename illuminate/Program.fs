module Program

open System
open System.Drawing
open System.Windows.Forms
open System.IO
open Newtonsoft.Json;
open Newtonsoft.Json.FSharp;
open Types
open Core
open Material
open Color
open Shape
open Ray
open Scene
open Light
open Camera
open Newtonsoft.Json.Linq

[<STAThread>]
do
    let spec = File.ReadAllText("../../SceneExample.json")
    let scene = JsonConvert.DeserializeObject<Scene>(spec)
    
    let image = buildScene scene

    // Setting up the UI components
    let mainForm = new Form(Width = scene.width, Height = scene.height, Text = "FRay")
    let box = new PictureBox(BackColor = Color.White, Dock = DockStyle.Fill, SizeMode = PictureBoxSizeMode.CenterImage)
    mainForm.Controls.Add(box)
    let bmp = new Bitmap(scene.width,scene.height)

    // render the scene
    let stopwatch = new System.Diagnostics.Stopwatch()
    Console.WriteLine("Building scene at resolution {0}*{1}...", scene.width, scene.height)
    stopwatch.Start()

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