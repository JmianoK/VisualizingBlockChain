﻿open System.IO
open System.Net

open Suave
open Suave.Operators
open Suave.Filters
open Suave.Successful
open Suave.RequestErrors
open Newtonsoft.Json
open Newtonsoft.Json.Serialization

open Shared

let clientPath = Path.Combine("..","Client") |> Path.GetFullPath 
let port = 8085us

let config =
  { defaultConfig with 
      homeFolder = Some clientPath
      bindings = [ HttpBinding.create HTTP (IPAddress.Parse "0.0.0.0") port ] }

let getInitCounter () : Async<Counter> = async { return 42 }

let init : WebPart = 
  Filters.path "/api/init" >=>
  fun ctx ->
    async {
      let! counter = getInitCounter()
      return! Successful.OK (string counter) ctx
    }


let JSON v =     
    let jsonSerializerSettings = new JsonSerializerSettings()
    jsonSerializerSettings.ContractResolver <- new CamelCasePropertyNamesContractResolver()

    JsonConvert.SerializeObject(v, jsonSerializerSettings)
    |> OK 
    >=> Writers.setMimeType "application/json; charset=utf-8"  

let fromJson<'a> json =
  JsonConvert.DeserializeObject(json, typeof<'a>) :?> 'a

let getResourceFromReq<'a> (req : HttpRequest) =
  let getString rawForm =
    System.Text.Encoding.UTF8.GetString(rawForm)
  req.rawForm |> getString |> fromJson<'a>

let sha256 : WebPart =
  choose [ 
    GET >=> 
    request (fun request ->
        match request.queryParam "message" with
        | Choice1Of2 message -> 
                  use sha256 = System.Security.Cryptography.SHA256.Create()
                  let hashedMessageHexadecimal = 
                    message 
                    |> System.Text.Encoding.UTF8.GetBytes
                    |> sha256.ComputeHash
                    |> Seq.map (fun c -> c.ToString("X2"))
                    |> Seq.reduce (+)
                  OK (sprintf "Hashed Message: %s" hashedMessageHexadecimal)
        | Choice2Of2 errorMessage -> BAD_REQUEST errorMessage)
    POST >=> 
    request (getResourceFromReq >> JSON)  
  ]       


let webPart =
  choose [
    init
    path "/" >=> Files.browseFileHome "index.html"
    sha256
    Files.browseHome
    RequestErrors.NOT_FOUND "Not found!"
  ]

startWebServer config webPart