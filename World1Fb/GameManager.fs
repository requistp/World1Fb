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
    abstract member Initialize : unit //: EntityComponentChange list
    abstract member Update: EntityComponentData -> unit //EntityComponentChange list

type Frame = {
    Number : uint32
    ECD : EntityComponentData
    ChangeLog : EntityComponentChange list
    }
    //static member New = Frame(0u,EntityComponentData(Map.empty,0u),List.empty)
    //static member Add number (tup:ChangesAndNewECData) = Frame(number, snd tup, fst tup)
    //member this.Number = number
    //member this.EntityComponentData = ecd
    //member this.ChangeLog = eccl

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

    let Initialize systems = 
        () //Frame.Add 0u (collectAndApplyChange (fun x -> x.IsActive) (fun s -> s.Initialize) (EntityComponentData.New) systems)

    let Update systems frame = 
        ()
        //let ecm = frame.EntityComponentData
        //Frame.Add (frame.Number+1u) (collectAndApplyChange (fun x -> x.IsActive && x.IsInitialized) (fun s -> s.Update ecm) ecm systems)

