module GameManager
open CommonGenericFunctions
open Components
open EntityComponentManager

type ChangesAndNewECData = EntityComponentChange list * EntityComponentData

[<AbstractClass>]
type AbstractSystem(isActive:bool) =
    let mutable _isInitialized = false

    member this.IsActive = isActive
    member this.IsInitialized = _isInitialized

    member internal this.SetToInitialized = _isInitialized <- true
    abstract member Initialize : unit
    abstract member Update: EntityComponentData -> unit //EntityComponentChange list

type Frame = {
    Number : uint32
    ECD : EntityComponentData
    ChangeLog : EntityComponentChange list
    }

module Game =
    let private applyChangeLog ecd eccl = 
        let mutable newecd = ecd
        for ecc in eccl do 
            match ecc with
            | EntityAddition ctl -> newecd <- Entity.Create newecd ctl
            | EntityRemoval e -> newecd <- Entity.Remove newecd e
            | _ -> () //newecm
        ChangesAndNewECData (eccl,newecd)

    let private collectAndApplyChange filterFx collectFx ecm (sl:AbstractSystem list) =
        sl
        |> List.filter filterFx
        |> List.collect collectFx
        |> applyChangeLog ecm
        
    let Initialize ecd (systems:AbstractSystem list) = 
        systems |> List.filter (fun x -> x.IsActive) |> List.iter (fun s -> s.Initialize)
        { Number=0u; ECD=ecd; ChangeLog=List.empty}

    let Update ecd systems frame = 
        ()
        //let ecm = frame.EntityComponentData
        //Frame.Add (frame.Number+1u) (collectAndApplyChange (fun x -> x.IsActive && x.IsInitialized) (fun s -> s.Update ecm) ecm systems)

