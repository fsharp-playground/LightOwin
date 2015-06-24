namespace Averruncus

[<AutoOpen>]
module Utils =
    let (?|>) x f = (Option.bind f) x
    let withDefault r l = match l with | Some l -> l | _ -> r
    let (?>) l r = withDefault r l
[<RequireQualifiedAccess>]
module Seq =
    let takeUntil predicate source =
        seq {
            let p = ref false
            for x in source do
                yield x, !p
                p := predicate x
        }
        |> Seq.takeWhile(fun(_, p) -> not p)
        |> Seq.map fst

[<AutoOpen>]
module Patterns =
    open System

    let (|List|_|) mapper source = 
        let x = source |> Seq.map mapper |> Seq.takeUntil Option.isNone |> Seq.toList
        match x |> List.rev with
        | Some _ :: _ -> Some(List.map Option.get x)
        | _ -> None
    let (|UrlTemplate|_|) template url =
        let template = UriTemplate(template)
        let u = Uri(url)
        let b = Uri(u.GetLeftPart(UriPartial.Authority))

        match template.Match(b, u) with
        | m when m <> null ->
            m.BoundVariables.Keys 
            |> Seq.cast<string> 
            |> Seq.map(fun x -> x, m.BoundVariables.Get x)
            |> Map.ofSeq 
            |> Some
        | _ -> None