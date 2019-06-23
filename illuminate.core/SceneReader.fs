namespace Illuminate
open Illuminate.Types
open FSharp.Json
open System.IO
open Illuminate.Ply

module SceneReader = 
    let readScene fileName =
        let addFace tm result face =
            let face0, face1, face2 = face
            let listVert = Seq.toList result.Vertices
            let v0x, v0y, v0z = listVert.[face0]
            let v1x, v1y, v1z = listVert.[face1]
            let v2x, v2y, v2z = listVert.[face2]
            Triangle {v0 = {x = v0x; y = v0y; z = v0z}; v1 = {x = v1x; y = v1y; z = v1z}; v2 = {x = v2x; y = v2y; z = v2z}; color = tm.color}
            
        let addTriangles tm result =
            let face = addFace tm result
            let faces = Seq.toList result.Faces
            faces |> List.map face

        let parseMesh shape =
            let tm = 
                match shape with 
                | TriangleMesh m -> m
                | _ -> {filePath = ""; triangles = list.Empty; color = {r = 0.; g = 0.; b = 0.}}

            let parsedFile = parsePLYFile tm.filePath

            match parsedFile with 
                | Success p -> addTriangles tm p
                | Failure s -> List.empty<Shape>

        let isMesh shape =
            match shape with 
                | TriangleMesh m -> true
                | _ -> false

        let checkForMesh scene =
            scene.shapes |> List.filter isMesh

        //start scene parsing
        let scene = Json.deserialize<Scene>(File.ReadAllText(fileName))
        let meshes = checkForMesh scene
        let triangles = List.concat (meshes |> (List.map parseMesh))
        { scene with shapes = List.append triangles scene.shapes }