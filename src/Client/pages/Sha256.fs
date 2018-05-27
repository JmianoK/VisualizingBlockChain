module Client.Sha256

open Elmish
open Fable.Import
open Fable.PowerPack
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Fable.Core.JsInterop
open Fable.PowerPack.Fetch.Fetch_types

type Model = {
  Value: string;
  ErrorMsg: string;
} 

type Msg =
    | GetHash
    | TextChanged of string
    | Success of Model
    | Error of exn

let getHash (model: Model) = 
  promise {
    let body = toJson model
    let props =
      [
        RequestProperties.Method HttpMethod.POST
        Fetch.requestHeaders [ HttpRequestHeaders.ContentType "application/json" ]
        RequestProperties.Body !^body
      ]
    try
      return! Fetch.fetchAs<Model> "http://localhost:8085/sha256" props
    with _ ->
      return! failwithf "Error"        
  }

let getHashCmd value = Cmd.ofPromise getHash value Success Error


let update (msg: Msg) model : Model*Cmd<Msg> = 
  printfn "CALLING UPDATE!!!!"
  printfn "%A" msg
  match msg with
  | TextChanged value -> 
    { model with Value = value }, Cmd.none
  | GetHash ->
    model, getHashCmd model 
  | Success model -> 
    printfn "==== SUCCESSSSSSS"
    { model with Value = model.Value }, Cmd.none
  | Error err ->
    printfn "==== ERROR"
    { model with ErrorMsg = err.Message }, Cmd.none


let view (model: Model) (dispatch: Msg -> unit) =
    [ div [ ]
        [ input [ Placeholder "Enter text to hash" 
                  Value model.Value
                  OnChange (fun (ev:React.FormEvent) -> dispatch (TextChanged !!ev.target?value)) ] ]
      div [ ]
        [
                button [ OnClick (fun _ -> dispatch GetHash) ] 
                       [ str "Get Hash" ]
        ]                                   
    ]                 

let init  =
  { 
    Value = ""
    ErrorMsg = ""
  }