module Client.Blockchain

open Fable.Helpers.React
open Fable.Helpers.React.Props
open Pages
open Client.Block
let view (model) =
     Block.view model

let update (msg: Msg) model = 
    printfn "pahingi"
    Block.update msg model

let init = {
    Block = "2"
    Nonce = "0"
    Text = { Value = "" }
    ErrorMsg = None
    HashValue = None
}  