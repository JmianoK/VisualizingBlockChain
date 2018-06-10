module Client.App

open Elmish
open Elmish.React

open Fable.Import.Browser
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Fable.PowerPack.Fetch
open Elmish.Browser.Navigation
open Fable.Import
open Pages

/// The composed model for the different possible page states of the application
type PageModel =
    | HomePageModel
    | Sha256Model of Sha256.Model
    | BlockPageModel of Block.Model
    | BlockchainPageModel of Block.Model

type Model = 
  { 
    PageModel: PageModel 
  }

type Msg =
| Sha256Msg of Sha256.Msg
| BlockMsg of Block.Msg
| BlockchainMsg of Block.Msg


// The navigation logic of the application given a page identity parsed from the .../#info  / information in the URL.
let urlUpdate (result:Page option) model =
    match result with
    | None ->
        Browser.console.error("Error parsing url: " + Browser.window.location.href)
        ( model, Navigation.modifyUrl (toHash Page.Home) )
    | Some Page.Sha256 ->
        { model with PageModel = Sha256Model Sha256.init }, Cmd.map Sha256Msg Cmd.none
    | Some Page.Home ->
        { model with PageModel = HomePageModel }, Cmd.none
    | Some Page.Block ->
        { model with PageModel = BlockPageModel Block.init }, Cmd.map BlockMsg Cmd.none
    | Some Page.Blockchain ->
        { model with PageModel = BlockchainPageModel Blockchain.init }, Cmd.map BlockchainMsg Cmd.none       


let init result = 
  let model = 
    { PageModel = HomePageModel }
  urlUpdate result model
  // let cmd =
  //   Cmd.ofPromise 
  //     (fetchAs<int> "/api/init") 
  //     [] 
  //     (Ok >> Init) 
  //     (Error >> Init)
  // model, cmd

let update msg (model : Model): Model*Cmd<Msg> =
    match msg, model.PageModel with
    | Sha256Msg msg, Sha256Model m ->
      let m, cmd = Sha256.update msg m
      // { model with PageModel = Sha256Model m }, Cmd.none // If we do this then promises are not called propagately
      { model with PageModel = Sha256Model m }, Cmd.map Sha256Msg cmd
    | BlockMsg msg, BlockPageModel m ->
      let m, cmd = Block.update msg m
      { model with PageModel = BlockPageModel m }, Cmd.map BlockMsg cmd        
    | BlockchainMsg msg, BlockchainPageModel m ->
      let m, cmd = Block.update msg m
      { model with PageModel = BlockchainPageModel m }, Cmd.map BlockchainMsg cmd     
    | _ -> model, Cmd.none

let safeComponents =
  let intersperse sep ls =
    List.foldBack (fun x -> function
      | [] -> [x]
      | xs -> x::sep::xs) ls []

  let components =
    [
      "Suave", "http://suave.io"
      "Fable", "http://fable.io"
      "Elmish", "https://fable-elmish.github.io/"
    ]
    |> List.map (fun (desc,link) -> a [ Href link ] [ str desc ] )
    |> intersperse (str ", ")
    |> span [ ]

  p [ ]
    [ strong [] [ str "SAFE Template" ]
      str " powered by: "
      components ]

/// Constructs the view for a page given the model and dispatcher.
let viewPage model dispatch =
    match model.PageModel with
    | HomePageModel -> Home.view ()
    | Sha256Model m -> Sha256.view m (Sha256Msg >> dispatch)
    | BlockPageModel m -> Block.view m (BlockMsg >> dispatch)
    | BlockchainPageModel m -> Blockchain.view m (BlockchainMsg >> dispatch)

let view model dispatch =
  div []
    [ 
      Menu.view
      div [] (viewPage model dispatch)
    ]
    
#if DEBUG
open Elmish.Debug
open Elmish.HMR
#endif

Program.mkProgram init update view
|> Program.toNavigable Pages.urlParser urlUpdate
#if DEBUG
|> Program.withConsoleTrace
|> Program.withHMR
#endif
|> Program.withReact "elmish-app"
#if DEBUG
|> Program.withDebugger
#endif
|> Program.run
