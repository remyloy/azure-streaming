// Learn more about F# at http://fsharp.org

open FParsec
open Suave
open Suave.Filters
open Suave.Operators
open Suave.Successful
open Suave.Sockets
open Suave.Sockets.Control
open System
open System.IO

module Choice =
    let mapLeft f c =
        match c with
        | Choice1Of2 x -> f x |> Choice1Of2
        | Choice2Of2 e -> Choice2Of2 e

    let bindLeft f c =
        match c with
        | Choice1Of2 x -> f x
        | Choice2Of2 e -> Choice2Of2 e

    let mapRight f c =
        match c with
            | Choice1Of2 s -> Choice1Of2 s
            | Choice2Of2 x -> f x |> Choice2Of2

    let unwrap c =
        match c with
        | Choice1Of2 x -> x
        | Choice2Of2 x -> x

module Parser =
    let prange : Parser<int64*int64 option, unit> =
        pipe2 pint64 (skipString "-" >>. (opt pint64)) (fun a b -> (a, b))

    let pranges : Parser<(int64*int64 option) list, unit> =
        pstring "bytes=" >>. (sepBy1 prange (pstring ","))

    let run header =
        run pranges header

    let ranges header =
        run header 
        |> function
            | Success (s,_,_) -> Choice1Of2 s
            | Failure (f,_,_) -> Choice2Of2 f


module Local =
    let streamFile filePath : WebPart =
        let parseRangeHeader rangeHeader ctx =
            Parser.ranges rangeHeader 
            |> Choice.mapRight (fun _ -> RequestErrors.BAD_REQUEST "invalid range header" ctx)

        let streamFileImpl ranges ctx =
            let task (rstart, rend) (conn, _) =
                socket {
                    use file = File.OpenRead(filePath)
                    let rend = rend |> Option.defaultValue (file.Length - 1L)
                    let documentLength = file.Length
                    let contentLength = 1L + rend - rstart
                    let! (_,conn) = asyncWriteLn "Accept-Ranges: bytes" conn
                    let! (_,conn) = asyncWriteLn (sprintf "Content-Length: %d" contentLength) conn
                    let! (_,conn) = asyncWriteLn (sprintf "Content-Range: bytes %d-%d/%d" rstart rend documentLength) conn
                    let! (_,conn) = asyncWriteLn "" conn
                    let! conn = flush conn
                    file.Position <- rstart
                    do! transferStream conn file
                    return conn
                }

            match ranges with
            | [x] ->
                let ctx' =
                    { ctx with
                        response = 
                            { ctx.response with 
                                status = HTTP_206.status
                                content = task x |> SocketTask } }
                succeed ctx'
            | _ ->
                ServerErrors.INTERNAL_ERROR "not supported" ctx

        fun ctx ->
            async {
                return! ctx.request.header "Range"
                |> Choice.mapRight (fun _ -> RequestErrors.BAD_REQUEST "missing range header" ctx)
                |> Choice.bindLeft (fun rangeHeader -> parseRangeHeader rangeHeader ctx)
                |> Choice.mapLeft (fun ranges -> streamFileImpl ranges ctx)
                |> Choice.unwrap
            }

let videoPath =
    FileInfo(Path.Combine(AppDomain.CurrentDomain.BaseDirectory, "..", "..", "..", "SampleVideo_1280x720_1mb.mp4")).FullName

let app =
    choose [
        GET >=> path "/stream.mp4" >=> Local.streamFile videoPath
        GET >=> OK "Hello World from F#!"
    ]

[<EntryPoint>]
let main argv =
    startWebServer defaultConfig app
    0 // return an integer exit code
