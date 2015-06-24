namespace Averruncus.Http

module Extensions = 

    open NLog
    open Averruncus.Http

    type HttpMessage with 
        member this.Log() =
            let logger = LogManager.GetCurrentClassLogger()
            sprintf "|| method -> %s" this.Method |> logger.Trace
            sprintf "|| header -> %A" this.Header |> logger.Trace
            sprintf "|| url -> %A" this.Url |> logger.Trace
            sprintf "|| body -> %A" this.Body |> logger.Trace
            sprintf "|| status -> %A" this.Status |> logger.Trace
        
