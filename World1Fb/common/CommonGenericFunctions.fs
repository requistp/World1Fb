﻿module CommonGenericFunctions
open System

type ComponentID = uint32
type ComponentTypeID = byte
type EntityID = uint32
type RoundNumber = uint32 

let rec searchArrayDataForRound (round:RoundNumber option) (arrayToSearch:(RoundNumber*'a option)[]) =
    match arrayToSearch with
    | [||] -> None
    | _ -> 
        match Array.head arrayToSearch with
        | r,c when round.IsNone || r <= round.Value -> c
        | _ -> 
            match Array.tail arrayToSearch with
            | [||] -> None
            | t -> searchArrayDataForRound round t

let castEnumToArray<'a> = (Enum.GetValues(typeof<'a>) :?> ('a [])) //This only works if the enum has been assigned int values
let castEnumToStringArray<'a> = Enum.GetNames(typeof<'a>) 

let random = Random(System.DateTime.Now.DayOfYear*1000000 + System.DateTime.Now.Hour*10000000 + System.DateTime.Now.Minute*100000 + System.DateTime.Now.Second*1000 + System.DateTime.Now.Millisecond)

let ArrayContentsMatch (a:'a[]) (b:'a[]) = 
    match a.Length = b.Length with
    | false -> false
    | true -> 
        (a |> Array.fold (fun s t -> s |> Array.filter (fun e -> e <> t )) b) = [||]

let MapKeys(map: Map<'K,'V>) =
    seq {
        for KeyValue(key,_) in map do
            yield key
    } |> Set.ofSeq

let MapValuesToArray (map:Map<'K,'V>) =
    seq {
        for KeyValue(_,value) in map do
            yield value
    } |> Set.ofSeq |> Seq.toArray

let TrueSomeFalseNone ifTrueFx statement =
    match statement with
    | false -> None
    | true -> Some ifTrueFx


let Map_Replace (map:Map<'K,'V>) (key:'K) (newValue:'V) =
    match map.ContainsKey(key) with
    | false -> map
    | true -> map.Remove(key).Add(key,newValue)

let Map_AppendValueToArrayNonUnique (map:Map<'K,'V[]>) (key:'K) (newValue:'V) =
    match map.ContainsKey(key) with
    | false -> map.Add(key,[|newValue|])
    | true -> 
        let a = map.Item(key) |> Array.append [|newValue|]
        map.Remove(key).Add(key,a)

let Map_AppendValueToArrayUnique (map:Map<'K,'V[]>) (key:'K) (newValue:'V) =
    match map.ContainsKey(key) with
    | false -> map.Add(key,[|newValue|])
    | true -> 
        match map.Item(key) |> Array.contains newValue with
        | true -> map
        | false ->
            let a = map.Item(key) |> Array.append [|newValue|]
            map.Remove(key).Add(key,a)

let Map_RemoveValueFromArray (map:Map<'K,'V[]>) (key:'K) (removeValue:'V) =
    match map.ContainsKey(key) with
    | false -> map
    | true -> let a = map.Item(key) |> Array.filter (fun v -> v <> removeValue)
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
    let Start (logger:string->unit) = 
        let st = System.DateTime.Now
        logger (sprintf "%s" (st.ToLongTimeString()))
        st
    let End (logger:string->unit) (name:string) (st:System.DateTime) =
        let et = System.DateTime.Now
        let cost = et.Subtract(st).Minutes*60000 + et.Subtract(st).Seconds*1000 + et.Subtract(st).Milliseconds
        logger (sprintf "%s %s-%s = %i" name (st.ToLongTimeString()) (et.ToLongTimeString()) cost)
        //printfn "\n%s start:%O, %i      " name st st.Millisecond
        //printfn "%s   end:%O, %i      " name et et.Millisecond
        //printfn "%s  cost:%i         " name (et.Subtract(st).Minutes*60000 + et.Subtract(st).Seconds*1000 + et.Subtract(st).Milliseconds)




//let bind switchFunction twoTrackInput = 
//    match twoTrackInput with
//    | Success s -> switchFunction s
//    | Failure f -> Failure f

//let (>>=) twoTrackInput switchFunction = 
//    bind switchFunction twoTrackInput 



