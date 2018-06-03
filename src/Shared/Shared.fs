module Shared

type HashValue = { HashedValue: string }

type ValueToHash = {
  Value: string;
} 

let getHashValue (hashValue: HashValue option) = 
    match hashValue with
    | Some value -> value.HashedValue
    | None -> ""