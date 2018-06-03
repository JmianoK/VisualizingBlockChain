module Shared

type HashValue = { HashedValue: string }

type ValueToHash = {
  Value: string;
} 

type MineResponse = {
  HashedValue: string;
  Block: string;
  Nonce: string;
}

let difficulty = 4
let maximumNonce = 500000

let rec generatePatternToFind valueToRepeat numberOfTimes =
    match numberOfTimes with
    | _ when numberOfTimes <= 0 -> ""
    | _ -> valueToRepeat + (generatePatternToFind valueToRepeat (numberOfTimes - 1))


let pattern = generatePatternToFind "0" difficulty

// Active Pattern To the rescue!
let (|Prefix|_|) (startsWith:string) (value:string) =
    if value.StartsWith(startsWith) then
        Some(value)
    else
        None

let (|FoundHash|_|) (startsWith:string) (value:string,_) =
    match value with
    | Prefix startsWith value-> Some(value)
    | _ ->  None
let (|LessThan|_|) k (_,value) = if value < k then Some() else None

let (|MoreThan|_|) k value = if value > k then Some() else None    

let getHashValue (hashValue: HashValue option) = 
    match hashValue with
    | Some value -> value.HashedValue
    | None -> ""