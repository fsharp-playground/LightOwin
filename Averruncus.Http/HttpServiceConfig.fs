namespace Averruncus.Http

open Unchecked
open System
open Averruncus
open Newtonsoft.Json

type ContentType = {
    Name: string
}
with
    member this.Serialize x = JsonConvert.SerializeObject x
    member this.Deserialize x = JsonConvert.DeserializeObject x :?> _
    member this.GetPostedParameters x: Map<string, string> option = None

[<AutoOpen>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module ContentType =
    let contentType x = { Name = x }

type HttpServiceConfig = {
    Method: string
    ContentType: ContentType
    Url: string
}
with
    static member MatchRequest(x: HttpRequest, c) =
        //GET, DELETE
        let getParams =
            match x.Url with
            | UrlTemplate c.Url p -> Some p
            | _ -> None
        //POST
        let postParams = c.ContentType.GetPostedParameters x.Body

        match [ getParams; postParams ] |> Seq.collect(Option.toList) |> Seq.toList with
        | [] -> None
        | x -> Seq.collect(Map.toSeq) x |> Map.ofSeq |> Some

    static member Serialize(x, c): string = 
        c.ContentType.Serialize x
    static member Deserialize(x, c) = 
        c.ContentType.Deserialize x