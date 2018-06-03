open System.IO
open System.Net

open Suave
open Suave.Operators
open Suave.Filters
open Suave.Successful
open Suave.Writers
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


// let init : WebPart = 
//   Filters.path "/api/init" >=>
//   fun ctx ->
//     async {
//       let! counter = getInitCounter()
//       return! Successful.OK (string counter) ctx
//     }

 
let JSON v =     
    let jsonSerializerSettings = new JsonSerializerSettings()
    // jsonSerializerSettings.ContractResolver <- new CamelCasePropertyNamesContractResolver()

    JsonConvert.SerializeObject(v, jsonSerializerSettings)
    |> OK 
    >=> Writers.setMimeType "application/json; charset=utf-8"  
 
let fromJson<'a> json =
  JsonConvert.DeserializeObject(json, typeof<'a>) :?> 'a

let getResourceFromReq<'a> (req : HttpRequest) =
  let getString rawForm =
    System.Text.Encoding.UTF8.GetString(rawForm)
  req.rawForm |> getString |> fromJson<'a>

let hashedMessageHexadecimal (message: string) = 
    use sha256 = System.Security.Cryptography.SHA256.Create()
    message
    |> System.Text.Encoding.UTF8.GetBytes
    |> sha256.ComputeHash
    |> Seq.map (fun c -> c.ToString("X2"))
    |> Seq.reduce (+)

let setCORSHeaders =
    setHeader  "Access-Control-Allow-Origin" "*"
    >=> setHeader "Access-Control-Allow-Headers" "content-type"

let allow_cors : WebPart =
    choose [
        OPTIONS >=>
            fun context ->
                context |> (
                    setCORSHeaders
                    >=> OK "CORS approved" )
    ]

let sha256 : WebPart =
  path "/sha256" >=> 
    choose [ 
        GET >=> 
        request (fun request ->
            match request.queryParam "message" with
            | Choice1Of2 message -> 
                      OK (sprintf "Hashed Message: %s" (hashedMessageHexadecimal message))
            | Choice2Of2 errorMessage -> BAD_REQUEST errorMessage)
        POST >=>       
                fun context ->
                    context |> 
                    (setCORSHeaders >=> 
                      request (getResourceFromReq<ValueToHash> 
                      >> (fun message -> hashedMessageHexadecimal message.Value) 
                      >> (fun hashedValue -> { HashedValue = hashedValue} ) 
                      >> JSON))
    ]


// 6 "0"s nonce is 17380171 for empty data with block 1
let rec mineWorker block nonce originalMessage = 
  let valueToHash = block.ToString() + nonce.ToString() + originalMessage
  let hashed = (hashedMessageHexadecimal valueToHash)
  match hashed with
    | Prefix "000000" hashed -> { HashedValue = hashed; Block = block.ToString(); Nonce = nonce.ToString() }
    | _ -> 
      match nonce with
      | LessThan maximumNonce -> mineWorker block (nonce + 1) originalMessage
      | _ -> { HashedValue = "Stopping mining since it's getting more expensive"; Block = block.ToString(); Nonce = nonce.ToString() }


let mine: WebPart =
  choose [
    POST >=> choose
     [ path "/mine" >=>
          (setCORSHeaders >=> 
                request(getResourceFromReq<ValueToHash> 
                >> (fun hashedValue -> (mineWorker 1 0 hashedValue.Value)) 
                >> JSON))
     ]
  ]       


let webPart =
  choose [
    allow_cors
    // init
    path "/" >=> Files.browseFileHome "index.html"
    sha256
    mine
    Files.browseHome
    RequestErrors.NOT_FOUND "Not found!"
  ]

startWebServer config webPart