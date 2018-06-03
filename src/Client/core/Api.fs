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


let getHashCmd value success error = Cmd.ofPromise getHash value success error