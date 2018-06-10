module Client.Blockchain

open Fable.Helpers.React
open Fable.Helpers.React.Props
open Pages
open Client.Block
open Shared
open Fable

type Model = {
    ItemSource: List<Block.Model>
}

// type Msg =
// | MsgFromChild of int * Block.Msg

let view (blockChainModel: Model) (dispatch: Msg -> unit) =
    printfn "IM IN BC VIEW"
    [
        ofList (
            blockChainModel.ItemSource
            |> List.collect (fun model -> Block.view model dispatch))
    ]    

let update (msg: Msg) (blockChainModel: Model) = 
    printfn "MSG: %A" msg
    printfn "MODEL: %A" blockChainModel.ItemSource

    blockChainModel.ItemSource
    |> List.map (fun model -> Block.update msg model)    


let init = {
    ItemSource = [
        {
            Id = 0
            Block = "0"
            Nonce = "0"
            Text = { Value = "" }
            ErrorMsg = None
            HashValue = None
            PreviousHash = Some { HashedValue = repeatValue "0" 64 }
            ShowPreviousHash = true
        };
        {
            Id = 1
            Block = "1"
            Nonce = "0"
            Text = { Value = "" }
            ErrorMsg = None
            HashValue = None
            PreviousHash = Some { HashedValue = repeatValue "0" 64 }
            ShowPreviousHash = true
        }
    ]
}