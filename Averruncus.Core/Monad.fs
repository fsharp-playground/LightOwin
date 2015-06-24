[<AutoOpen>]
module Monad
    open System

    type MaybeMonad() =
        member this.Bind(m, f) = m |> Option.bind f
        member this.Return x = Some x
        member this.ReturnFrom m = m
    let option = MaybeMonad()

    type StateMonad() =
        member this.Delay f = fun s -> f() s
        member this.Bind(m, f) = fun s -> m s ||> f
        member this.ReturnFrom m = m
        member this.Return x = fun s -> x, s
        member this.Zero() = fun s -> s, s
        member this.Get() = fun s -> s, s
        member this.Set s = fun _ -> (), s
    let state = StateMonad()
    
    type ConfigurationMonad() =
        member this.Delay f = fun c -> f() c
        member this.Bind(m, f) = fun c -> (m c, c) ||> f
        member this.ReturnFrom m = m
        member this.Return x = fun c -> x
        member this.Get() = fun c -> c
    let config = ConfigurationMonad()
    
    [<RequireQualifiedAccess>]
    module State =
        let initWith s x = x s |> fst

    [<RequireQualifiedAccess>]
    module Config =
        let supply c x = x c
        let rearrange f = fun c x -> f x c
        let map x = fun c -> x
        let fmap f = fun x c -> x c |> f
        let ntran fc = fun cf cc ->
            config {
                let! config = config.Get()
                let! c = cc
                let f a = cf (map a) config

                return fc f c
            }
        let inline bind f x = fun c -> (x c, c) ||> f
        let inline fbind f = bind(fmap f)
    
    type ContinuationMonad() =
        member this.Delay f = fun c -> f() c
        member this.Bind(m, f) = fun c -> m (fun x -> f x c)
        member this.Return x = fun c -> c x
        member this.ReturnFrom m = m
    let cont = ContinuationMonad()

    type Either<'t, 'u> = Left of 't | Right of 'u
    type Result<'t, 'u when 'u :> Exception> = Value of 't | Error of 'u

    type ErrorMonad() =
        member this.Bind(m, f) = 
            match m with
            | Value x -> f x
            | Error x -> Error x
        member this.Return x = Value x
        member this.ReturnFrom m = m
        member this.Yield x = Error x
        member this.Zero() = Value ()
    let error = ErrorMonad()