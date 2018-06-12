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

let view (blockChainModel: Model) (dispatch: Msg -> unit) =
    [
        div [ flexRow ]
            [
                ofList (
                    blockChainModel.ItemSource
                    |> List.collect (fun model -> Block.view model dispatch))
            ]
    ]        

let getUpdateFromChild msg blockChainModel blockModel= 
    let childUpdate = 
        blockChainModel.ItemSource
        |> List.map(fun item -> 
                        if item.Id = blockModel.Id then
                            Block.update msg
                        else
                            (item, Cmd.none))
    (childUpdate |> List.map(fun (item, _) -> item), childUpdate |> List.map(fun (_, cmd) -> cmd) |> Cmd.batch)

let update (msg: Msg) (blockChainModel: Model) = 
    match msg with
    | TextChanged blockModel 
    | NonceChanged blockModel
    | Mine blockModel
    | UpdatePreviousHash blockModel
    | FasterMine blockModel -> 
        getUpdateFromChild msg blockChainModel blockModel
    | GetHashMine (blockModel, _)
    | GetHashResponse (blockModel, _)
    | Error (blockModel, _) ->
        getUpdateFromChild msg blockChainModel blockModel
    | GetFasterMineResponse (blockModel, _) ->
        let childUpdate = 
            blockChainModel.ItemSource
            |> List.map(fun item -> 
                            printfn "Getting mine %A" item.Id
                            if item.Id = blockModel.Id then
                                Block.update msg
                            else
                                match item.HashValue with
                                | Some value ->
                                    match value.HashedValue with
                                    | Prefix pattern _ -> (item, Cmd.ofMsg(UpdatePreviousHash item))
                                    | _ -> (item, Cmd.ofMsg(FasterMine item))
                                | None -> (item, Cmd.ofMsg(FasterMine item)))
        (childUpdate |> List.map(fun (item, _) -> item), childUpdate |> List.map(fun (_, cmd) -> cmd) |> Cmd.batch)

let generateInitialData id =
 {
    Id = id
    Block = id.ToString()
    Nonce = "0"
    Text = { Value = "" }
    ErrorMsg = None
    HashValue = None
    PreviousHash = Some { HashedValue = repeatValue "0" 64 }
    ShowPreviousHash = true
 }

let init = { ItemSource = [1..3] |> List.map (fun i -> generateInitialData i) }