[<RequireQualifiedAccess>]
module Utilitarian.Widgets.Generic.ListModel

type T<'Model> = 'Model list

type Msg<'Model,'ModelMsg> = 
  | Prepend of 'Model
  | Add of Position:int * 'Model
  | Remove of Position:int 
  | DispatchTo of Position:int * Message: 'ModelMsg

let init () = []

let update (modelUpdater : 'ModelMsg -> 'Model -> 'Model) (listMsg : Msg<_,_>) (listModel : T<_>) = 

  match listMsg with 

    | DispatchTo (i,msg) -> 
      listModel |> List.mapi (fun j model ->
        if i = j then modelUpdater msg model else model)

    | Prepend model -> model::listModel

    | Remove i -> 
        let rec remove j rest = [ 
          match rest with
          | head::nextRest -> 
            if i<>j then
              yield head
              yield! remove (j+1) nextRest
          | [] -> yield! []
        ]
        remove 0 listModel

    | Add (i, model) -> 
        let rec add j rest = [ 
          match rest with
          | head::nextRest -> 
            if i=j then
              yield model; yield head
              yield! add (j+1) nextRest            
          | [] -> yield! []
        ]
        add 0 listModel