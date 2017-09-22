// Learn more about F# at http://fsharp.org

open Suave
open Suave.Filters
open Suave.Operators
open Suave.Successful
open System

let app =
    GET >=> OK "Hello World from F#!"

[<EntryPoint>]
let main argv =
    startWebServer defaultConfig app
    0 // return an integer exit code
