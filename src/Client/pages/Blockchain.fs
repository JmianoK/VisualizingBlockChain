module Client.Blockchain

open Fable.Helpers.React
open Fable.Helpers.React.Props
open Elmish
open Pages
open Client.Block
open Shared
open Styles
open Fable

type Model = {
    ItemSource: List<Block.Model>
}

// type Msg =
// | MsgFromChild of int * Block.Msg

let view (blockChainModel: Model) (dispatch: Msg -> unit) =
    printfn "[Blockchain View]"
    [
        div [ flexRow ]
            [
                ofList (
                    blockChainModel.ItemSource
                    |> List.collect (fun model -> Block.view model dispatch))
            ]
    ]        

let update (msg: Msg) (blockChainModel: Model) = 
    printfn "[Blockchain Update]"
    match msg with
    | NonceChanged model
    | TextChanged model -> 
        let childUpdate = 
            blockChainModel.ItemSource
            |> List.map(fun item -> 
                            if item.Id = model.Id then
                                Block.update msg item
                            else
                                (item, Cmd.none))
        (childUpdate |> List.map(fun (item, _) -> item), childUpdate |> List.map(fun (_, cmd) -> cmd) |> Cmd.batch)
    | Success (model, _) ->
        let childUpdate = 
            blockChainModel.ItemSource
            |> List.map(fun item -> 
                            if item.Id = model.Id then
                                Block.update msg item
                            else
                                (item, Cmd.none))
        (childUpdate |> List.map(fun (item, _) -> item), childUpdate |> List.map(fun (_, cmd) -> cmd) |> Cmd.batch)
                                
    | _ ->    
        [],[] 


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
        };
        {
            Id = 2
            Block = "2"
            Nonce = "0"
            Text = { Value = "" }
            ErrorMsg = None
            HashValue = None
            PreviousHash = Some { HashedValue = repeatValue "0" 64 }
            ShowPreviousHash = true
        }
    ]
}