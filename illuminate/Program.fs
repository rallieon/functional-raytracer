module Program

open System
open System.Drawing
open System.Windows.Forms
open System.IO
open Newtonsoft.Json;
open Types
open Core
open Scene

[<STAThread>]
do
    let spec = File.ReadAllText("../../SceneExample.json")
    let scene = JsonConvert.DeserializeObject<Scene>(spec)
    
    let image = buildScene scene

    let mainForm = new Form(Width = scene.width, Height = scene.height, Text = "FRay")
    let box = new PictureBox(BackColor = Color.White, Dock = DockStyle.Fill, SizeMode = PictureBoxSizeMode.CenterImage)
    mainForm.Controls.Add(box)
    let bmp = new Bitmap(scene.width,scene.height)

    for x in 0..(image.Length-1) do
        for y in 0..(image.[x].Length-1) do 
            bmp.SetPixel(x,y,image.[x].[y])

    bmp.Save("FRayOutput.jpg");

    box.Image <- (bmp :> Image)
    Application.Run(mainForm)