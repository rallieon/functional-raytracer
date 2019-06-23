namespace Illuminate

open System.IO
open System.Text.RegularExpressions

#nowarn "25"

module Ply =
    type Vertice = float * float * float
    type Face = int * int * int

    let readLines (fullPath:string) = 
        seq {
            use sr = new StreamReader (fullPath)
            while not sr.EndOfStream do
                yield sr.ReadLine() 
        }

    type ParserResult = 
        {
            VertexCount : int
            FaceCount : int
            FaceProperties : string * string
            VertexProperties : (string * string) list
            ObjectInfo: (string * string) list
            Vertices: seq<Vertice>
            Faces: seq<Face>
        }
        static member Init()=   
            {
                VertexCount = 0
                FaceCount = 0
                FaceProperties = ("","")
                VertexProperties =[]
                ObjectInfo = []
                Vertices = Seq.empty
                Faces = Seq.empty
            }
    
    type ParserSuccess<'a> =
        | Success of 'a
        | Failure of string

    let map f aPS=
        match aPS with 
        | Success( a )-> f a |> Success
        | Failure s -> Failure s

    let combine xPS yPS = 
        match (xPS,yPS) with
        | Success(x),Success(y) -> Success(x,y)
        | _ -> Failure <| sprintf "Can not combine %A %A" xPS yPS

    let bind f aPS = 
        match aPS with
        | Success x -> f x
        | Failure s -> Failure s

    
    let outerSuccess<'a> (seqIn: ParserSuccess<'a> seq) = 
        let containsFailure = 
            seqIn
            |>Seq.exists (fun (elPS) -> 
                    match elPS with 
                    | Failure _ -> true
                    | _ -> false)
        match containsFailure with
        | true ->
            Failure ("Could be a litte bit more precise: Failure in " + (typeof<'a>).ToString())
        | false ->
            Success( Seq.map (fun s -> match s with | Success(v) -> v) seqIn)

    //exception ParseError of string;;

    /// <summary>Checks whether a string matches a certain regex.</summary>
    /// <param name="s">The string to check.</param>
    /// <param name="r">The regex to match.</param>
    /// <returns>Whether or not the string matches the regex.</returns>
    let matchesRegex s r = Regex.Match(s, r).Success

    /// <summary>Parse the header of a PLY file into predefined, mutable values.</summary>
    /// <param name="header">A sequence of the header lines in a PLY file, not including "end_header".</param>
    /// <exception cref="ParseError">Raised when the input is not recognized as anything useful.</exception>
    let parseHeader (header: seq<string>) = 
        let parseHeaderRaw accPS (line:string) = 
            match accPS with 
            | Failure (_) -> accPS
            | Success (parserResult) -> 
                let splitted = line.Split[|' '|]
                match line with
                | x when matchesRegex x @"obj_info .*"                ->
                    let a = Array.item 1 splitted
                    let b = Array.item 2 splitted
                    {  parserResult with ObjectInfo = parserResult.ObjectInfo@[(a, b)]} |> Success
                | x when matchesRegex x @"element vertex \d*"         ->
                    {  parserResult with VertexCount =  int (Array.item 2 splitted)} |> Success
                | x when matchesRegex x @"property list .*"           ->
                    let a = Array.item 2 splitted
                    let b = Array.item 3 splitted
                    {  parserResult with FaceProperties =  (a, b)}
                    |> Success
                | x when matchesRegex x @"property .*"                ->
                    let a = Array.item 1 splitted
                    let b = Array.item 2 splitted
                    {  parserResult with VertexProperties =  parserResult.VertexProperties@[(a, b)]}
                    |> Success
                | x when matchesRegex x @"element face \d*"           ->
                    {  parserResult with FaceCount =  int (Array.item 2 splitted)}
                    |> Success
                | x when ((x = "ply") || matchesRegex x @"format .*") -> Success parserResult
                | x when matchesRegex x @"comment .*" -> Success parserResult
                | _                                                   -> 
                    Failure "Malformed header."  

        header |> Seq.fold parseHeaderRaw  (ParserResult.Init() |> Success)

    let stringToVertice (s: string) =
        match s with
        | s when s.Length < 3 -> 
            System.Console.WriteLine(s)
            sprintf "Malformed vertices: %s" s |> Failure 
        | _ -> 
            let splitted = s.Split[|' '|]
            let pick  i = Array.item i splitted
            let x = pick 0
            let y = pick 1
            let z = pick 2
            (float x, float y, float z) |> Success

    let parseVertices (vertices: seq<string>) = Seq.map stringToVertice vertices |> outerSuccess

    let stringToFace (s: string) =
        match s with
        | s when s.Length < 3 -> 
            System.Console.WriteLine(s)
            sprintf "Malformed vertices: %s" s |> Failure                             
        | _ -> 
            let splitted = s.Split[|' '|]
            let x = Array.item 0 splitted
            let y = Array.item 1 splitted
            let z = Array.item 2 splitted
            (int x, int y, int z) |> Success

    let parseFaces (faces: seq<string>) =
        faces |> Seq.map stringToFace |> outerSuccess

    let parsePLYFile fileName =
        let lines = readLines fileName
        // At which index is the header located? The vertices? The faces?
        let bodyPos = lines |> Seq.findIndex(fun a -> a = "end_header")
        let header = lines |> Seq.take bodyPos

        // Parse the header, the vertices & the faces.
        parseHeader header
        |> bind (fun resultHeaderPS -> 
                let faces = lines |> Seq.skip (bodyPos + resultHeaderPS.VertexCount + 1) |> Seq.take  resultHeaderPS.FaceCount |> parseFaces
                let vertices = lines |> Seq.skip (bodyPos + 1) |> Seq.take resultHeaderPS.VertexCount |> parseVertices

                combine vertices faces
                |> map(fun (vertices, faces) ->
                        {   resultHeaderPS with Vertices = vertices; Faces = faces }  ) )