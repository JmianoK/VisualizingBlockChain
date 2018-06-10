module Client.Blockchain

open Fable.Helpers.React
open Fable.Helpers.React.Props
open Pages
open Client.Block
open Shared
open Fable

let view (model) (dispatch: Msg -> unit) =
    [ 
        ofList (List.concat [ for _ in 0 .. 5 do yield (Block.view model dispatch) ])
    ]    

let update (msg: Msg) model = 
    printfn "test"
    Block.update msg model

let init = {
    Block = "2"
    Nonce = "0"
    Text = { Value = "" }
    ErrorMsg = None
    HashValue = None
    PreviousHash = Some { HashedValue = repeatValue "0" 64 }
    ShowPreviousHash = true
}  