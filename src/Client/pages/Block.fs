module Client.Block

open Fable.Helpers.React
open Fable.Helpers.React.Props
open Fable.Import
open Elmish
open Fable.Core.JsInterop
open Client.Styles
open Shared


type Model = {
  Id: int;
  Block: string;
  Nonce: string;
  Text: ValueToHash;
  ErrorMsg: string option;
  HashValue: HashValue option;
  PreviousHash: HashValue option;
  ShowPreviousHash: bool;
} 

type Msg =
    | TextChanged of int * ValueToHash
    | NonceChanged of int * string
    | Mine
    | GetFasterMine
    | FasterMineResponse of MineResponse
    | Success of HashValue
    | GetHashMine of HashValue
    | Error of exn


let update (msg: Msg) model : Model*Cmd<Msg> = 
  printfn "Block Update: %A" msg
  match msg with
  | TextChanged (_, valueToHash) -> 
    { model with Text = valueToHash }, Api.getCmd Api.getHash { Value = model.Block + model.Nonce + valueToHash.Value } Success Error
  | NonceChanged (_, nonceValue) -> 
    { model with Nonce = nonceValue }, Api.getCmd Api.getHash { Value = model.Block + nonceValue + model.Text.Value } Success Error
  | Mine ->
    { model with HashValue = None }, Api.getCmd Api.getHash { Value = model.Block + model.Nonce + model.Text.Value } GetHashMine Error
  | GetHashMine hashedValue -> 
    match hashedValue.HashedValue with
    | Prefix pattern _ -> { model with HashValue = Some { HashedValue = hashedValue.HashedValue } }, Cmd.none
    | _ -> 
        { model with HashValue = Some { HashedValue = hashedValue.HashedValue }; Nonce = ((model.Nonce |> int) + 1).ToString() }, Cmd.ofMsg(Mine)
  | GetFasterMine -> { model with HashValue = None }, Api.getCmd Api.mineNonce model.Text FasterMineResponse Error  
  | FasterMineResponse mineResponse -> { model 
                                         with Nonce = mineResponse.Nonce; 
                                              HashValue = Some { HashedValue = mineResponse.HashedValue } 
                                       }, Cmd.none
  | Success hashedValue -> 
    printfn "SUCCESS IS CALLED"
    { model with HashValue = Some { HashedValue = hashedValue.HashedValue } }, Cmd.none
  | Error err ->
    { model with ErrorMsg = Some err.Message }, Cmd.none

let shouldAddPreviousHash model = 
  match model.ShowPreviousHash with
  | true ->
              div [ centerStyle "Row" ] // I can also do if not show then yield Hidden
                [ span [ ]
                    [ str "Previous Hash:" ]
                  input [ Value (getHashValue model.PreviousHash)
                          ReadOnly true
                          Style [ Width !!"100%" ] ] ] |> Some
  | false ->  None

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
                  OnChange (fun (ev:React.FormEvent) -> dispatch (NonceChanged (model.Id, !!ev.target?value)))
                  Style [ Width !!"100%" ] ] ]              
      div [ centerStyle "Row" ]
        [ span [ ]
            [ str "Data:" ]
          textarea [ 
                  Placeholder "Enter text to hash" 
                  Cols 50.
                  Rows 20.                                    
                  Value model.Text.Value
                  OnChange (fun (ev:React.FormEvent) -> dispatch (TextChanged  (model.Id, { Value = !!ev.target?value })))
                  Style [ Width !!"100%" ]
        ] [ ] ] 
      ( ofOption (shouldAddPreviousHash model))
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
      ]   
    ]           
                  
let init = {
    Id = 0
    Block = "1"
    Nonce = "0"
    Text = { Value = "" }
    ErrorMsg = None
    HashValue = None
    PreviousHash = None
    ShowPreviousHash = false
}  