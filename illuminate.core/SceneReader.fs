namespace Illuminate
open Illuminate.Types
open FSharp.Json
open System.IO

module SceneReader = 
    let readScene fileName =
        Json.deserialize<Scene>(File.ReadAllText(fileName))