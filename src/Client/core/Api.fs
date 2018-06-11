module Client.Api


open Elmish
open Fable.PowerPack
open Fable.Core.JsInterop
open Fable.PowerPack.Fetch.Fetch_types
open Shared

let getHash (model: ValueToHash) = 
  promise {
    let body = toJson model
    let props =
      [
        RequestProperties.Method HttpMethod.POST
        Fetch.requestHeaders [ HttpRequestHeaders.ContentType "application/json" ]
        RequestProperties.Body !^body
      ]
    try
      return! Fetch.fetchAs<HashValue> "http://localhost:8085/sha256" props
    with _ ->
      return! failwithf "Error"        
  }


let mineNonce (model: ValueToHash) = 
  promise {
    let body = toJson model
    let props =
      [
        RequestProperties.Method HttpMethod.POST
        Fetch.requestHeaders [ HttpRequestHeaders.ContentType "application/json" ]
        RequestProperties.Body !^body
      ]
    try
      return! Fetch.fetchAs<MineResponse> "http://localhost:8085/mine" props
    with _ ->
      return! failwithf "Error"        
  }


let getCmd apiToExecute value success error = 
  printfn "DID U EVEN TRY"
  Cmd.ofPromise apiToExecute value success error
