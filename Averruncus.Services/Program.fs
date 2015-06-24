// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.
open Microsoft.Owin.Hosting
open System.Linq
open System.Collections.Generic
open System
open Microsoft.Owin;
open Averruncus
open Averruncus.Http
open Averruncus.Services

[<EntryPoint>]
let main argv = 
    use srv = WebApp.Start<HttpServer>(new StartOptions(url = "http://localhost:9000"))

    printfn "The server has been started..."
    printfn "Use 'q' to exit."

    Seq.initInfinite(fun _ -> Console.ReadLine()) 
    |> Seq.map(fun x -> x.Trim()) 
    |> Seq.where(fun x -> x <> "") 
    |> Seq.takeWhile(fun x -> x <> "q")
    |> Seq.iter(fun _ -> ())

    0 // return an integer exit code
