module Client.Styles

open Fable.Helpers.React.Props
open Fable.Core
open Fable.Core.JsInterop
open Fable.Import
open Fable.PowerPack
open Shared
module R = Fable.Helpers.React


let getBackgroundColor (hashedValue: HashValue option) = 
    match hashedValue with
    | Some value -> 
        match value.HashedValue with
        | Prefix pattern _ -> "#81C784"
        | _ -> "#e57373"
    | None -> ""

let viewLink page description =
  R.a [ Style [ Padding "0 20px" ]
        Href (Pages.toHash page) ]
      [ R.str description]
      
let centerStyle direction =
    Style [ Display "flex"
            FlexDirection direction
            AlignItems "center"
            !!("justifyContent", "center")
            Padding "20px 0"
    ]

let flexRow =
    Style [ Display "flex"
            FlexDirection "row"
    ]
