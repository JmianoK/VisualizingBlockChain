module Client.App

open Elmish
open Elmish.React

open Fable.Import.Browser
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Fable.PowerPack.Fetch
open Elmish.Browser.Navigation
open Fable.Import
open Shared
open Pages

/// The composed model for the different possible page states of the application
type PageModel =
    | HomePageModel
    | Sha256Model

type Model = 
  { 
    // Counter: Counter option
    PageModel: PageModel }

type Msg =
| Increment
| Decrement
| Init of Result<Counter, exn>


// The navigation logic of the application given a page identity parsed from the .../#info  / information in the URL.
let urlUpdate (result:Page option) model =
    match result with
    | None ->
        Browser.console.error("Error parsing url: " + Browser.window.location.href)
        ( model, Navigation.modifyUrl (toHash Page.Home) )
    | Some Page.Sha256 ->
        { model with PageModel = Sha256Model }, Cmd.none
    | Some Page.Home ->
        { model with PageModel = HomePageModel }, Cmd.none


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

let update msg (model : Model) =
  // let model' =
  //   match msg with
  //   | Some x, Increment -> Some (x + 1)
  //   | Some x, Decrement -> Some (x - 1)
  //   | None, Init (Ok x) -> Some x
  //   | _ -> None
  // model', Cmd.none
  model, Cmd.none

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


open Fable.Helpers.React
open Fable.Helpers.React.Props
/// Constructs the view for a page given the model and dispatcher.
let viewPage model dispatch =
    match model.PageModel with
    | HomePageModel -> Home.view ()
    | Sha256Model  -> Sha256.view ()

let show = function
| Some x -> string x
| None -> "Loading..."

let view model dispatch =
  div []
    [ h1 [] [ str "SAFE Template" ]
      p  [] [ str "The initial counter is fetched from server" ]
      p  [] [ str "Press buttons to manipulate counter:" ]
      // button [ OnClick (fun _ -> dispatch Decrement) ] [ str "-" ]
      // div [] [ str (show model) ]
      // button [ OnClick (fun _ -> dispatch Increment) ] [ str "+" ]
      div [] (viewPage model dispatch)
      // safeComponents ]
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
