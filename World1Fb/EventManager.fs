module EventManager
open GameEvents


type listenerCallback = AbstractGameEvent -> unit


type EventManager() =
    let mutable _listeners = Map.empty:Map<GameEventTypes,listenerCallback[]> 
    let mutable _pendingEvents = Array.empty<AbstractGameEvent>

    member private this.processEventBatch = 
        let processCallbacks (ge:AbstractGameEvent) = 
            match _listeners.ContainsKey(ge.GameEventType) with
            | false -> ()
            | true -> _listeners.Item(ge.GameEventType) |> Array.iter (fun cb -> cb ge)

        let processedEvents = _pendingEvents
        _pendingEvents <- Array.empty
        processedEvents |> Array.iter (fun ge -> processCallbacks ge)
        processedEvents

    member this.ProcessEvents = 
        let mutable processedEvents = Array.empty<AbstractGameEvent>
        while (_pendingEvents.Length > 0) do
            processedEvents <- Array.append processedEvents this.processEventBatch
        processedEvents

    member _.QueueEvent ge = 
        _pendingEvents <- Array.append _pendingEvents [|ge|]

    member _.RegisterListener et callback = 
        match _listeners.ContainsKey(et) with
        | false -> _listeners <- _listeners.Add(et,[|callback|])
        | true -> let l = Array.append (_listeners.Item(et)) [|callback|]
                  _listeners <- _listeners.Remove(et).Add(et,l)

