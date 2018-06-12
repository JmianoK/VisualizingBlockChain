module Client.Blockchain

open Fable.Helpers.React
open Fable.Helpers.React.Props
open Elmish
open Pages
open Client.Block
open Shared
open Styles
open Fable
open Client

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
    | SiblingFasterMine blockModel
    | Mine blockModel ->
        getUpdateFromChild msg blockChainModel blockModel
    | FasterMine blockModel -> 
        printfn "Faster mining %A" blockModel.Id
        getUpdateFromChild msg blockChainModel blockModel
    | GetHashMine (blockModel, _)
    | GetHashResponse (blockModel, _)
    | GetFasterMineSiblingResponse (blockModel, _)
    | Error (blockModel, _) ->
        getUpdateFromChild msg blockChainModel blockModel

    | UpdatePreviousHash blockModel -> printfn "Updating previoushash from %A" blockModel.Id
                                       getUpdateFromChild msg blockChainModel blockModel
    | GetFasterMineResponse (blockModel, _) ->
        let childUpdate = 
            blockChainModel.ItemSource
            |> List.map(fun item -> 
                            if item.Id = blockModel.Id then
                                Block.update msg
                            else
                                match item.HashValue with
                                | Some value ->
                                    match value.HashedValue with
                                    | Prefix pattern _ -> (item,Cmd.none)
                                    | _ -> (item, Cmd.ofMsg(SiblingFasterMine item))
                                | None -> (item, Cmd.ofMsg(SiblingFasterMine item)))
        
        printfn "[%A]: Mining Response" blockModel.Id
        let a = childUpdate |> List.map(fun (item,_) -> (item.Id, item.HashValue))
        printfn "[%A]: %A" blockModel.Id a                            
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

let init = { ItemSource = [1..100] |> List.map (fun i -> generateInitialData i) }