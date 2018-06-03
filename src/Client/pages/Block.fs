module Client.Block

open Fable.Helpers.React
open Fable.Helpers.React.Props
open Fable.Import
open Elmish
open Fable.Core.JsInterop
open Client.Styles
open Shared


type Model = {
  Block: string;
  Nonce: string;
  Text: ValueToHash;
  ErrorMsg: string option;
  HashValue: HashValue option;
} 

type Msg =
    | TextChanged of ValueToHash
    | NonceChanged of string
    | Mine
    | GetFasterMine
    | FasterMineResponse of MineResponse
    | Success of HashValue
    | GetHashMine of HashValue
    | Error of exn


let update (msg: Msg) model : Model*Cmd<Msg> = 
  match msg with
  | TextChanged valueToHash -> 
    { model with Text = valueToHash }, Api.getCmd Api.getHash { Value = model.Block + model.Nonce + valueToHash.Value } Success Error
  | NonceChanged nonceValue -> 
    { model with Nonce = nonceValue }, Api.getCmd Api.getHash { Value = model.Block + nonceValue + model.Text.Value } Success Error
  | Mine ->
    model, Api.getCmd Api.getHash { Value = model.Block + model.Nonce + model.Text.Value } GetHashMine Error
  | GetHashMine hashedValue -> 
    match hashedValue.HashedValue with
    | Prefix pattern _ -> { model with HashValue = Some { HashedValue = hashedValue.HashedValue } }, Cmd.none
    | _ -> 
        { model with HashValue = Some { HashedValue = hashedValue.HashedValue }; Nonce = ((model.Nonce |> int) + 1).ToString() }, Cmd.ofMsg(Mine)
  | GetFasterMine -> model, Api.getCmd Api.mineNonce model.Text FasterMineResponse Error  
  | FasterMineResponse mineResponse -> { model 
                                         with Nonce = mineResponse.Nonce; 
                                              HashValue = Some { HashedValue = mineResponse.HashedValue } 
                                       }, Cmd.none
  | Success hashedValue -> 
    { model with HashValue = Some { HashedValue = hashedValue.HashedValue } }, Cmd.none
  | Error err ->
    { model with ErrorMsg = Some err.Message }, Cmd.none

let view (model: Model) (dispatch: Msg -> unit) =
    [ div [ Style [ Background !! (getBackgroundColor model.HashValue) ] ] 
    [ 
          div [ centerStyle "Row" ]
            [ span [ ]
                [ str "Block:" ]
              input [ Value (model.Block.ToString())
                      ReadOnly true
                      Style [ Width !!"100%" ] ] ]  
          div [ centerStyle "Row" ]
            [ span [ ]
                [ str "Nonce:" ]
              input [ Value (model.Nonce.ToString())
                      OnChange (fun (ev:React.FormEvent) -> dispatch (NonceChanged !!ev.target?value))
                      Style [ Width !!"100%" ] ] ]              
          div [ centerStyle "Row" ]
            [ span [ ]
                [ str "Data:" ]
              textarea [ 
                      Placeholder "Enter text to hash" 
                      Cols 50.
                      Rows 20.                                    
                      Value model.Text.Value
                      OnChange (fun (ev:React.FormEvent) -> dispatch (TextChanged  { Value = !!ev.target?value }))
                      Style [ Width !!"100%" ]
            ] [ ] ] 
          div [ centerStyle "Row" ]
            [ span [ ]
                [ str "Hash:" ]
              input [ Value (getHashValue model.HashValue)
                      ReadOnly true
                      Style [ Width !!"100%" ] ] ]
          div [ centerStyle "Row" ]
            [ button [ OnClick (fun _ -> dispatch Mine) ]
                [ str "Mine" ] 
              button [ OnClick (fun _ -> dispatch GetFasterMine) ]
                [ str "Faster Mine" ]   
            ]
    ]   ]           


let init = {
    Block = "1"
    Nonce = "0"
    Text = { Value = "" }
    ErrorMsg = None
    HashValue = None
}  