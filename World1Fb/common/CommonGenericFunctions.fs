﻿module CommonGenericFunctions
open System


let random = Random(System.DateTime.Now.DayOfYear*1000000 + System.DateTime.Now.Hour*10000000 + System.DateTime.Now.Minute*100000 + System.DateTime.Now.Second*1000 + System.DateTime.Now.Millisecond)


let Keys(map: Map<'K,'V>) =
    seq {
        for KeyValue(key,value) in map do
            yield key
    } |> Set.ofSeq


let TrueSomeFalseNone ifTrueFx statement =
    match statement with
    | false -> None
    | true -> Some ifTrueFx


let Map_Replace (map:Map<'K,'V>) (key:'K) (newValue:'V) =
    match map.ContainsKey(key) with
    | false -> map
    | true -> map.Remove(key).Add(key,newValue)


let Map_AppendValueToArray (map:Map<'K,'V[]>) (key:'K) (newValue:'V) =
    match map.ContainsKey(key) with
    | false -> map.Add(key,[|newValue|])
    | true -> let a = map.Item(key) |> Array.append [|newValue|]
              map.Remove(key).Add(key,a)


let OptionBindNone (noneFx:'T option->'T option) (last:'T option) =
    match last with
    | None -> noneFx None
    | Some _ -> last


let ResolveCombiningTwoOptions (o1:'T option) (o2:'T option) =
    match o1 with
    | None -> o2
    | Some s1 -> match o2 with
                 | None -> o1
                 | Some s2 -> if s1=s2 then o1 else None

module Timer =
    let Start = System.DateTime.Now
    let End name (st:System.DateTime) =
        let et = System.DateTime.Now
        printfn "\n%s start:%O, %i" name st st.Millisecond
        printfn "%s   end:%O, %i" name et et.Millisecond
        printfn "%s  cost:%i" name (et.Subtract(st).Milliseconds)




//let bind switchFunction twoTrackInput = 
//    match twoTrackInput with
//    | Success s -> switchFunction s
//    | Failure f -> Failure f

//let (>>=) twoTrackInput switchFunction = 
//    bind switchFunction twoTrackInput 


