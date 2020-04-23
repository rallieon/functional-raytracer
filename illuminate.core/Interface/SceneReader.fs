namespace Illuminate.Interface
open Illuminate.Framework.Types
open Illuminate.Framework.Math
open FSharp.Json
open System.IO
open Illuminate.Interface.Ply
open FsAlg.Generic

module SceneReader = 
    let readScene fileName =
        let addFace tm result face =
            let face0, face1, face2 = face
            let listVert = Seq.toList result.Vertices
            let v0x, v0y, v0z, n0x, n0y, n0z = listVert.[face0]
            let v1x, v1y, v1z, n1x, n1y, n1z = listVert.[face1]
            let v2x, v2y, v2z, n2x, n2y, n2z = listVert.[face2]

            let N = 
                match n0x = 0., n0y = 0., n0z = 0. with
                    | true, true, true -> None
                    | _ -> 
                        let unnormalized = vector [(n0x + n1y + n2z) / 3.; (n0y + n1y + n1y) / 3.; (n0z + n1z + n2z) / 3. ]
                        Some(unnormalized |> Vector.unitVector)

            let result = {
                v0 = vector [v0x; v0y; v0z]; 
                v1 = vector [v1x; v1y; v1z]; 
                v2 = vector [v2x; v2y; v2z]; 
                color = tm.color;
                triangleNormal = N;
                transformation = tm.transformation
            }

            //adds data needed for vector to be multiplied by 4x4 translation matrix
            //exteremely important that arr comes before [|1|]
            //https://www.euclideanspace.com/maths/geometry/affine/matrix4x4/index.htm
            let addFourthDimension v =
                let arr = Vector.toArray v
                let arrWith1 = Array.append arr [|1.|]
                let vec = Vector.ofArray arrWith1
                vec

            //transform the vector by the translation matrix
            //have to convert vector to 4th dimension first
            let transformVector trans v = 
                let vWithFourth = addFourthDimension v
                let transformedV = trans.value * vWithFourth
                transformedV.[..2]

            //TODO Comment
            match result.transformation with
                | None -> Triangle result
                | Some trans ->
                    Triangle {
                        v0 = transformVector trans result.v0
                        v1 = transformVector trans result.v1
                        v2 = transformVector trans result.v2
                        color = result.color;
                        triangleNormal = result.triangleNormal;
                        transformation = result.transformation
                    }
            
        let addTriangles tm result =
            let face = addFace tm result
            let faces = Seq.toList result.Faces
            faces |> List.map face

        let parseMesh shape =
            let tm = 
                match shape with 
                | TriangleMesh m -> m
                | _ -> {filePath = ""; triangles = list.Empty; color = {r = 0.; g = 0.; b = 0.}; transformation = None}

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