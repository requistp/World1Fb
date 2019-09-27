module CommonGenericFunctions

//type Result<'TSuccess,'TFailure> = 
//    | Success of 'TSuccess
//    | Failure of 'TFailure

//let IsFailure (r:Result<'TSuccess,'TFailure>) = 
//    match r with 
//    | Failure _ -> true
//    | Success _ -> false
//let FailurePart (r:Result<'TSuccess,'TFailure>) = 
//    match r with 
//    | Failure f -> Some f
//    | Success _ -> None
    
//let IsSuccess (r:Result<'TSuccess,'TFailure>) = 
//    match r with 
//    | Failure _ -> false
//    | Success _ -> true
//let SuccessPart (r:Result<'TSuccess,'TFailure>) = 
//    match r with 
//    | Failure _ -> None
//    | Success s -> Some s

//let bind switchFunction twoTrackInput = 
//    match twoTrackInput with
//    | Success s -> switchFunction s
//    | Failure f -> Failure f

//let (>>=) twoTrackInput switchFunction = 
//    bind switchFunction twoTrackInput 

let Keys(map: Map<'K,'V>) =
    seq {
        for KeyValue(key,value) in map do
            yield key
    } |> Set.ofSeq
