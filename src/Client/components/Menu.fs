module Client.Menu

open Fable.Helpers.React
open Client.Styles
open Pages

let view =
    div [ centerStyle "row" ] [
          yield viewLink Page.Home "Home"
          yield viewLink Page.Sha256 "SHA256"
          yield viewLink Page.Block "Block"
        ]