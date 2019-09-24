module CommonGenericFunctions

type Result<'TSuccess,'TFailure> = 
    | Success of 'TSuccess
    | Failure of 'TFailure

let bind switchFunction twoTrackInput = 
    match twoTrackInput with
    | Success s -> switchFunction s
    | Failure f -> Failure f

let (>>=) twoTrackInput switchFunction = 
    bind switchFunction twoTrackInput 

let Keys(map: Map<'K,'V>) =
    seq {
        for KeyValue(key,value) in map do
            yield key
    } |> Set.ofSeq
