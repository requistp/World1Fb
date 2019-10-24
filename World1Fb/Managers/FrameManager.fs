module FrameManager
open AbstractComponent
open EventManager
open EventTypes
open System

type Frame = 
    {
        Number : uint32
        Entities : Map<uint32,AbstractComponent[]>
        MaxEntityID : uint32 
        GEResults : GameEventResult[]
    } with 
    static member empty = 
        { 
            Number = 0u
            Entities = Map.empty
            MaxEntityID = 0u
            GEResults = Array.empty
        }

type GEListType = 
    | All
    | AllExceptFirst
    | Current
    | Last10Frames
    | Last10FramesExcludingFirst

type FrameManager() =
    let mutable _frames = [| Frame.empty |]
    
    member this.AddFrame (entities:Map<uint32,AbstractComponent[]>) (maxEntityID:uint32) (geResults:(EventData_Generic * Result<string option,string>)[]) =
        _frames <- 
            [| 
                { 
                    Number = (uint32 _frames.Length)
                    Entities = entities
                    MaxEntityID = maxEntityID
                    GEResults = geResults
                } 
            |]
            |> Array.append _frames
    
    member this.Count = _frames.Length
    member this.Frames = _frames
    member this.GameEventsAll (lt:GEListType) = 
        let start = 
            match lt with
            | All -> 0u
            | AllExceptFirst -> 2u
            | Current -> (uint32 (_frames.Length-1))
            | Last10Frames -> (uint32 (Math.Clamp(_frames.Length-10,0,Int32.MaxValue)))
            | Last10FramesExcludingFirst -> (uint32 (Math.Clamp(_frames.Length-10,2,Int32.MaxValue)))
        _frames 
        |> Array.filter (fun f -> f.Number >= start)
        |> Array.sortBy (fun f -> f.Number)
        |> Array.collect (fun f -> [|(f.Number,f.GEResults)|])

    member this.GERs_ToString (lt:GEListType) = 
        let gerToString (n:uint32) ((age,res):GameEventResult) =
            let printRes (res:Result<string option,string>) =
                match res with
                | Ok o -> "Ok "
                | Error s -> "Err"
            let printResString (res:Result<string option,string>) =
                match res with
                | Error s -> sprintf "/ %s" s
                | Ok o -> match o with
                          | None -> ""
                          | Some s -> sprintf "/ %s" s
            sprintf "%i|%s: %s %s\n" n (printRes res) age.ToString (printResString res)
        let gersToString (n:uint32) (gers:GameEventResult[]) =
            gers
            |> Array.fold (fun s ger -> s + (gerToString n ger)) ""
        this.GameEventsAll lt
        |> Array.fold (fun s (n,gers) -> s + (gersToString n gers)) ""

