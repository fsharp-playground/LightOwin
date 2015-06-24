namespace Averruncus.Http

open System
open Unchecked
open System.Linq.Expressions
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Linq.RuntimeHelpers.LeafExpressionConverter
open Averruncus

type HttpMessage = {
    Method: string
    Url: string
    Header: Map<string, string list>
    Body: string
    Status: int
}
//with
//    static member ToMessage(x: HttpMessage): Message = { Locator = ""; Content = "" }
//    static member ToMessage(x: Message): HttpMessage = { Method = "GET"; Url = ""; Header = Map.empty; Body = "" }

type HttpRequest = HttpMessage
type HttpResponse = HttpMessage

[<AutoOpen>]
module Component =
    let private response = { Method = ""; Body = ""; Header = Map.empty; Url = ""; Status = 0 }
    let notFound = { response with Status = 404 }
    let ok = { response with Status = 200 }

[<RequireQualifiedAccess>]
module Http =
    let getFuncArgInfo x =
        match x with
        | Lambda(u, expr) when u.Type = typeof<unit> -> 
            [ "" ]
        | Lambda(_, Lambda(_, Call(_, _, List(|Var|_|) args))) -> 
            args |> List.map(fun x -> x.Name)
        | _ -> 
            raise(Exception("Unknown")) 
    let inline private matchRequest x: 't -> _ =
        config {
            let! c = config.Get()
            return (^t: (static member MatchRequest: HttpRequest * _ -> Map<string, string> option) (x, c))
        }
    let inline private serialize x: 't -> _ =
        config { 
            let! c = config.Get()
            return (^t: (static member Serialize: _ * _ -> string) (x, c))
        }
    let inline private deserialize(x: string option): 'c -> 'r option = 
        config { 
            let! c = config.Get()
            if typeof<unit> = typeof<'r> then 
                return Some defaultof<_>
            else
                return x ?|> fun x -> Some ((^c or ^r): (static member Deserialize: string * ^c -> ^r) (x, c))
        }
    let getFunction(f: ('p -> 'r) Expr) =
        let f = QuotationToExpression f
        let f = f :?> MethodCallExpression
        let f = f.Arguments.[0] :?> LambdaExpression

        if typeof<'r> = typeof<unit> then
            let f = Expression.Lambda<Action<'p>>(f.Body, f.Parameters).Compile()
            fun p ->
                f.Invoke p
                defaultof<_>
        else
            Expression.Lambda<Func<'p, 'r>>(f.Body, f.Parameters).Compile().Invoke
        

    [<RequireQualifiedAccess>]
    module Service =
        let inline fmap(f: ('p -> 'r) Expr): 'config -> _ -> _ =
            let p = getFuncArgInfo f
            let f = getFunction f
            fun rq ->
                config {
                    let! rq = matchRequest rq
                    match rq with
                    | Some rq ->
                        let! p = Map.tryFind p.[0] rq |> deserialize
                        match p with
                        | Some p ->
                            let! r = f p |> serialize
                            return Some { ok with Body = r }
                        | _ -> return None
                    | _ -> 
                        return None
                }
            |> Config.rearrange
//        let inline fmap2(f: 'p0 -> 'p1 -> 'r): 'config -> _ -> _ =
//            fun rq ->
//                config {
//                    let p = getFuncArgInfo <@@ f @@>
//                    let! rq = matchRequest rq
//                    match rq with
//                    | Some rq ->
//                        let! p0 = Map.tryFind p.[0] rq |> deserialize
//                        let! p1 = Map.tryFind p.[1] rq |> deserialize
//                        match p0, p1 with
//                        | Some p0, Some p1 ->
//                            let! r = f p0 p1 |> serialize
//                            return Some { ok with Body = r }
//                        | _ -> return None
//                    | _ -> 
//                        return None
//                }
//            |> Config.rearrange
//        let inline fmap3(f: 'p0 -> 'p1 -> 'p2 -> 'r): 'config -> _ -> _ =
//            fun rq ->
//                config {
//                    let p = getFuncArgInfo <@@ f @@>
//                    let! rq = matchRequest rq
//                    match rq with
//                    | Some rq ->
//                        let! p0 = Map.tryFind p.[0] rq |> parse
//                        let! p1 = Map.tryFind p.[1] rq |> parse
//                        let! p2 = Map.tryFind p.[2] rq |> parse
//                        let! r = f p0 p1 p2 |> toResponse
//                        return Some r
//                    | _ -> 
//                        return None
//                }
//            |> Config.rearrange
//        let inline fmap4(f: 'p0 -> 'p1 -> 'p2 -> 'p3 -> 'r): 'config -> _ -> _ =
//            fun rq ->
//                config {
//                    let p = getFuncArgInfo <@@ f @@>
//                    let! rq = matchRequest rq
//                    match rq with
//                    | Some rq ->
//                        let! p0 = Map.tryFind p.[0] rq |> parse
//                        let! p1 = Map.tryFind p.[1] rq |> parse
//                        let! p2 = Map.tryFind p.[2] rq |> parse
//                        let! p3 = Map.tryFind p.[3] rq |> parse
//                        let! r = f p0 p1 p2 p3 |> toResponse
//                        return Some r
//                    | _ -> 
//                        return None
//                }
//            |> Config.rearrange
//        let inline fmap5(f: 'p0 -> 'p1 -> 'p2 -> 'p3 -> 'p4 -> 'r): 'config -> _ -> _ =
//            fun rq ->
//                config {
//                    let p = getFuncArgInfo <@@ f @@>
//                    let! rq = matchRequest rq
//                    match rq with
//                    | Some rq ->
//                        let! p0 = Map.tryFind p.[0] rq |> parse
//                        let! p1 = Map.tryFind p.[1] rq |> parse
//                        let! p2 = Map.tryFind p.[2] rq |> parse
//                        let! p3 = Map.tryFind p.[3] rq |> parse
//                        let! p4 = Map.tryFind p.[4] rq |> parse
//                        let! r = f p0 p1 p2 p3 p4 |> toResponse
//                        return Some r
//                    | _ -> 
//                        return None
//                }
//            |> Config.rearrange
//        let inline fmap6(f: 'p0 -> 'p1 -> 'p2 -> 'p3 -> 'p4 -> 'p5 -> 'r): 'config -> _ -> _ =
//            fun rq ->
//                config {
//                    let p = getFuncArgInfo <@@ f @@>
//                    let! rq = matchRequest rq
//                    match rq with
//                    | Some rq ->
//                        let! p0 = Map.tryFind p.[0] rq |> parse
//                        let! p1 = Map.tryFind p.[1] rq |> parse
//                        let! p2 = Map.tryFind p.[2] rq |> parse
//                        let! p3 = Map.tryFind p.[3] rq |> parse
//                        let! p4 = Map.tryFind p.[4] rq |> parse
//                        let! p5 = Map.tryFind p.[5] rq |> parse
//                        let! r = f p0 p1 p2 p3 p4 p5 |> toResponse
//                        return Some r
//                    | _ -> 
//                        return None
//                }
//            |> Config.rearrange
//        let inline fmap7(f: 'p0 -> 'p1 -> 'p2 -> 'p3 -> 'p4 -> 'p5 -> 'p6 -> 'r): 'config -> _ -> _ =
//            fun rq ->
//                config {
//                    let p = getFuncArgInfo <@@ f @@>
//                    let! rq = matchRequest rq
//                    match rq with
//                    | Some rq ->
//                        let! p0 = Map.tryFind p.[0] rq |> parse
//                        let! p1 = Map.tryFind p.[1] rq |> parse
//                        let! p2 = Map.tryFind p.[2] rq |> parse
//                        let! p3 = Map.tryFind p.[3] rq |> parse
//                        let! p4 = Map.tryFind p.[4] rq |> parse
//                        let! p5 = Map.tryFind p.[5] rq |> parse
//                        let! p6 = Map.tryFind p.[6] rq |> parse
//                        let! r = f p0 p1 p2 p3 p4 p5 p6 |> toResponse
//                        return Some r
//                    | _ -> 
//                        return None
//                }
//            |> Config.rearrange
//[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
//module Channel =
//    open Channel
////    let send: (Channel -> Request -> Response) = fun c rq -> ""
////    let sendHttp: (Channel -> (Channel -> Request -> Response) -> HttpRequest -> HttpResponse) = fun c p rq ->
////        { Header = ""; Body = "" } 
////        |> HttpMessage.op_Explicit 
////        |> p c 
////        |> HttpMessage.op_Explicit
////    let receiveHttp: (Channel -> HttpRequest -> HttpResponse) = fun c rq -> { Header = ""; Body = "" } 
////    let receive: (Channel -> (Channel -> HttpRequest -> HttpResponse) -> Request -> Response) = fun c p rq ->
////        rq 
////        |> HttpMessage.op_Explicit 
////        |> p c 
////        |> HttpMessage.op_Explicit
//    let httpProtocol: Channel -> HttpMessage -> HttpMessage = messaging (fun c (x: Message) -> x)