module CommonGenericFunctions
open System

let random = Random(System.DateTime.Now.Hour*10000000 + System.DateTime.Now.Minute*100000 + System.DateTime.Now.Second*1000 + System.DateTime.Now.Millisecond)

let Keys(map: Map<'K,'V>) =
    seq {
        for KeyValue(key,value) in map do
            yield key
    } |> Set.ofSeq

let TrueSomeFalseNone ifTrueFx statement =
    match statement with
    | false -> None
    | true -> Some ifTrueFx

module Timer =
    let Start = System.DateTime.Now
    let End name (st:System.DateTime) =
        let et = System.DateTime.Now
        printfn "\n%s start:%O, %i" name st st.Millisecond
        printfn "%s   end:%O, %i" name et et.Millisecond
        printfn "%s  cost:%i" name (et.Subtract(st).Milliseconds)


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
