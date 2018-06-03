module Client.Sha256

open Elmish
open Fable.Import
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Fable.Core.JsInterop
open Client.Styles
open Shared

type Model = {
  Text: ValueToHash;
  ErrorMsg: string option;
  HashValue: HashValue option;
} 

type Msg =
    | TextChanged of ValueToHash
    | Success of HashValue
    | Error of exn

let update (msg: Msg) model: Model * Cmd<Msg> = 
  match msg with
  | TextChanged valueToHash -> 
    { model with Text = valueToHash }, Api.getCmd Api.getHash valueToHash Success Error
  | Success msg -> 
    { model with HashValue = Some { HashedValue = msg.HashedValue } }, Cmd.none
  | Error err ->
    { model with ErrorMsg = Some err.Message }, Cmd.none

let view (model: Model) (dispatch: Msg -> unit) =
    [ div [ centerStyle "Row" ]
        [ span [ ]
            [ str "Data:" ]
          textarea [ 
                  Placeholder "Enter text to hash" 
                  Cols 50.
                  Rows 20.                                    
                  Value model.Text.Value
                  OnChange (fun (ev:React.FormEvent) -> dispatch (TextChanged { Value = !!ev.target?value }))
                  Style [ Width !!"100%" ]
        ] [ ] ] 
      div [ centerStyle "Row" ]
        [ span [ ]
            [ str "Hash:" ]
          input [ Value (getHashValue model.HashValue)
                  ReadOnly true
                  Style [ Width !!"100%" ] ] ]
    ]                 

let init  =
  { 
    Text = { Value = "" }
    ErrorMsg = None
    HashValue = None
  }