namespace Averruncus.Http

open Owin
open Microsoft.Owin;
open System
open Unchecked
open Averruncus
open Averruncus.Http

[<AutoOpen>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module OwinExts =
    open NLog
    open Averruncus.Http.Extensions
    let logger = LogManager.GetCurrentClassLogger()

    let private ofOwinRequest (x: IOwinRequest) = { 
        Method = x.Method
        Url = x.Uri.ToString()
        Header = x.Headers |> Seq.map(fun x -> x.Key, x.Value |> Array.toList) |> Map.ofSeq
        Body = ""
        Status = 0
    }
    type IAppBuilder with
        member this.RunServices(services: (HttpRequest -> HttpResponse option) list) =
            this.Run(fun context ->
                let rq = context.Request |> ofOwinRequest

                // trace
                rq.Log()

                let rs =
                    match services |> Seq.choose(fun x -> rq |> x) |> Seq.truncate 1 |> Seq.toList with 
                    | [] -> notFound
                    | rs :: _ -> rs
                context.Response.StatusCode <- rs.Status
                context.Response.ContentType <- "application/json; charset=utf-8"
//                    Map.tryFind "Content-Type" rs.Header ?> [] 
//                    |> Seq.fold(fun s x -> match s with | Some s -> Some(s + " ;" + x) | _ -> None) (Some "") ?> ""
//                    |> function | "" -> "" | x -> x.Substring(2)
                context.Response.WriteAsync(rs.Body)
            )
