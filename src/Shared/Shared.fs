module Shared

type HashValue = { HashedValue: string }

type ValueToHash = {
  Value: string;
} 

let difficulty = 4
let maximumNonce = 500000

let rec generatePatternToFind valueToRepeat numberOfTimes =
    match numberOfTimes with
    | _ when numberOfTimes <= 0 -> ""
    | _ -> valueToRepeat + (generatePatternToFind valueToRepeat (numberOfTimes - 1))


let pattern = generatePatternToFind "0" difficulty

// Active Pattern To the rescue!
let (|Prefix|_|) (p:string) (s:string) =
    if s.StartsWith(p) then
        Some(s)
    else
        None

let getHashValue (hashValue: HashValue option) = 
    match hashValue with
    | Some value -> value.HashedValue
    | None -> ""