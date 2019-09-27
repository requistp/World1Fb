module GameManager
open CommonGenericFunctions
open Components
open EntityComponentManager
open SystemManager

[<Struct>]
type Frame(number:uint32, ecm:EntityComponentManager, ccl:ComponentChangeLog) =
    static member New = Frame(0u, EntityComponentManager(), ComponentChangeLog Map.empty)
    member this.Number = number
    member this.EntityManager = ecm
    member this.Add (ecm:EntityComponentManager) (ccl:ComponentChangeLog) = Frame(number + 1u, ecm, ccl)
     
type Game(frames:Frame list) =
    let mutable _frames = frames
    let frame_Add (ecm:EntityComponentManager) (ccl:ComponentChangeLog) = _frames <- [_frames.Head.Add ecm ccl] @ frames; _frames.Head
    let systemManager = SystemManager()
    
    member this.EntityManager = _frames.Head.EntityManager
    member this.Frames = _frames
    member this.Frame_Current = frames.Head

    member this.InitializeGame (sl:AbstractSystem list) = 
        //Could check for game already initialized, but until that, I'm just gonna clear existing frams...
        _frames <- [Frame.New]
        match systemManager.RegisterSystems this.EntityManager sl with
        | Failure x -> Failure x
        | Success ecm -> Success (frame_Add ecm)

    //member this.Update ecman = 
    //    match isInitialized with
    //    | false -> Failure "Game is not initialized"
    //    | true  -> Success (frame_Add ecman)
        //Run all systems to generate the new state of all Entities (ie, it should return an ECM which would be piped in below)
        //  _frames.Head.ECM 
        //  |> SystemManager.Update 
        //  |> frame_Add
    
    new() = Game([Frame.New])
