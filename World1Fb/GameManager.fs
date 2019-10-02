module GameManager
open CommonGenericFunctions
open Components
open EntityComponentManager
open System

type ChangesAndNewECData = EntityComponentChange list * Map<Guid,EntityComponent list>

[<AbstractClass>]
type AbstractSystem(isActive:bool) =
    let mutable _isInitialized = false

    member this.IsActive = isActive
    member this.IsInitialized = _isInitialized

    member internal this.SetToInitialized = _isInitialized <- true
    abstract member Initialize : unit
    abstract member Update: Map<Guid,EntityComponent list> -> unit //EntityComponentChange list

type Frame = {
    Number : uint32
    ECD : Map<Guid,EntityComponent list>
    ChangeLog : EntityComponentChange list
    }

module Game =
    let private applyChangeLog (ecd:Map<Guid,EntityComponent list>) eccl = 
        let mutable newecd = ecd
        for ecc in eccl do 
            match ecc with
            | EntityAddition e -> newecd <-newecd.Add(fst e, snd e)
            | EntityRemoval e -> newecd <- newecd.Remove(e)
            | _ -> () //newecm
        ChangesAndNewECData (eccl,newecd)

    let private collectAndApplyChange filterFx collectFx ecm (sl:AbstractSystem list) =
        sl
        |> List.filter filterFx
        |> List.collect collectFx
        |> applyChangeLog ecm
        
    let Initialize (ecd:Map<Guid,EntityComponent list>) (systems:AbstractSystem list) = 
        systems |> List.filter (fun x -> x.IsActive) |> List.iter (fun s -> s.Initialize)
        { Number=0u; ECD=ecd; ChangeLog=List.empty}

    let Update ecd systems frame = 
        ()
        //let ecm = frame.EntityComponentData
        //Frame.Add (frame.Number+1u) (collectAndApplyChange (fun x -> x.IsActive && x.IsInitialized) (fun s -> s.Update ecm) ecm systems)

