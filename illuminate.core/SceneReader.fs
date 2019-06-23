namespace Illuminate
open Illuminate.Types
open FSharp.Json
open System.IO
open Illuminate.Ply

module SceneReader = 
    let parseMesh scene shape =
        let tm = 
            match shape with 
            | TriangleMesh m -> m
            | _ -> {filePath = ""; triangles = list.Empty; color = {r = 0.; g = 0.; b = 0.}}

        let parsedFile = parsePLYFile tm.filePath
        0

    let readScene fileName =
        let scene = Json.deserialize<Scene>(File.ReadAllText(fileName))
        let addMesh = parseMesh scene

        let isMesh shape =
            match shape with 
                | TriangleMesh m -> true
                | _ -> false

        let checkForMesh scene =
            scene.shapes |> List.filter isMesh

        let meshes = checkForMesh scene
        let result = meshes |> List.map addMesh
        scene