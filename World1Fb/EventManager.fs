module EventManager
open CommonGenericFunctions
open GameEvents


type listenerCallback = AbstractGameEvent -> unit


type EventListenerDictionary() =
    let mutable _listeners = Map.empty:Map<GameEventTypes,listenerCallback[]> 
    member this.ContainsKey et = _listeners.ContainsKey et
    member this.Item et = _listeners.Item et
    member this.RegisterListener et callback = _listeners <- Map_AppendValueToArray _listeners et callback


type PendingEventsDictionary() =
    let mutable _pending = Array.empty<AbstractGameEvent>
    member this.CollectAndClear = 
        let p = _pending
        _pending <- Array.empty
        p
    member this.IsEmpty = (_pending.Length = 0)
    member this.QueueEvent ge = _pending <- Array.append _pending [|ge|]


type EventManager() =
    let listeners = new EventListenerDictionary() 
    let pending = new PendingEventsDictionary()

    member private this.processEventBatch = 
        let processCallbacks (ge:AbstractGameEvent) = 
            match listeners.ContainsKey ge.GameEventType with
            | false -> ()
            | true -> listeners.Item ge.GameEventType |> Array.iter (fun cb -> cb ge) // Seems risky to Parallel since I don't know what the callbacks will do

        let processed = pending.CollectAndClear
        processed |> Array.iter (fun ge -> processCallbacks ge) // Seems risky to Parallel since I don't know what the callbacks will do
        processed

    member this.ProcessEvents = 
        let mutable processedEvents = Array.empty<AbstractGameEvent>
        while (not pending.IsEmpty) do
            processedEvents <- Array.append processedEvents this.processEventBatch
        processedEvents

    member this.RegisterListener et callback = listeners.RegisterListener et callback
    member this.QueueEvent ge = pending.QueueEvent ge

