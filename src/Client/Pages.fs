module Client.Pages

open Elmish.Browser.UrlParser

/// The different pages of the application. If you add a new page, then add an entry here.
[<RequireQualifiedAccess>]
type Page = 
    | Home
    | Sha256
    | Block

let toHash =
    function
    | Page.Home -> "#home"
    | Page.Sha256 -> "#sha256"
    | Page.Block -> "#block"


/// The URL is turned into a Result.
let pageParser : Parser<Page -> Page,_> =
    oneOf
        [ map Page.Home (s "home")
          map Page.Sha256 (s "sha256")
          map Page.Block (s "block") ]

let urlParser location = parseHash pageParser location