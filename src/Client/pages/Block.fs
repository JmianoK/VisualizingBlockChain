module Client.Block

open Fable.Helpers.React
open Fable.Helpers.React.Props
open Fable.Import
open Elmish
open Fable.Core.JsInterop
open Client.Styles
open Shared
open Client

type Block = {
  Id: int;
  Block: string;
  Nonce: string;
  Text: ValueToHash;
  ErrorMsg: string option;
  HashValue: HashValue option;
  PreviousHash: HashValue option;
  ShowPreviousHash: bool;
}

type Model = Block

type Msg =
    | TextChanged of Model
    | NonceChanged of Model
    | Mine of Model
    | FasterMine of Model
    | GetFasterMineResponse of Model * MineResponse
    | GetHashResponse of Model * HashValue
    | GetHashMine of Model * HashValue
    | Error of Model * exn

let hasSolvedProblem model (hashedValue: HashValue) = 
 match hashedValue.HashedValue with
    | Prefix pattern _ -> { model with HashValue = Some hashedValue }, Cmd.none
    | _ -> 
        let model = { model with HashValue = Some hashedValue; Nonce = ((model.Nonce |> int) + 1).ToString() }
        model, Cmd.ofMsg(Mine model)
      
let tupleCommandMessage msg updatedModel = (fun hashedValue -> msg (updatedModel, hashedValue))

let getHashCmd model textToHash successMsg = Api.getCmd Api.getHash textToHash (tupleCommandMessage successMsg model) (tupleCommandMessage Error model)
let getMineNonceCmd model = Api.getCmd Api.mineNonce model.Text (tupleCommandMessage GetFasterMineResponse model) (tupleCommandMessage Error model) 

let update (msg: Msg) = 
  match msg with
  | TextChanged model ->
    model, getHashCmd model { Value = model.Block + model.Nonce + model.Text.Value } GetHashResponse
  | NonceChanged model ->
    { model with Nonce = model.Nonce }, getHashCmd model { Value = model.Block + model.Nonce + model.Text.Value } GetHashResponse
  | Mine model ->
    model, getHashCmd model { Value = model.Block + model.Nonce + model.Text.Value } GetHashMine
  | FasterMine model -> 
    { model with HashValue = None }, getMineNonceCmd model
  | GetHashMine (model, hashedValue) -> 
    hasSolvedProblem model hashedValue
  | GetFasterMineResponse (model, mineResponse) ->
    { model with Nonce = mineResponse.Nonce; HashValue = Some { HashedValue = mineResponse.HashedValue } }, Cmd.none
  | GetHashResponse (model, hashedValue) -> 
    { model with HashValue = Some { HashedValue = hashedValue.HashedValue } }, Cmd.none
  | Error (model, err) -> 
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
                  OnChange (fun (ev:React.FormEvent) -> dispatch (NonceChanged { model with Nonce = !!ev.target?value }))
                  Style [ Width !!"100%" ] ] ]              
      div [ centerStyle "Row" ]
        [ span [ ]
            [ str "Data:" ]
          textarea [ 
                  Placeholder "Enter text to hash" 
                  Cols 50.
                  Rows 20.                                    
                  Value model.Text.Value
                  OnChange (fun (ev:React.FormEvent) -> dispatch (TextChanged  { model with Text = { Value = !!ev.target?value }}))
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
        [ button [ OnClick (fun _ -> dispatch (Mine model)) ]
            [ str "Mine" ] 
          button [ OnClick (fun _ -> dispatch (FasterMine model)) ]
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