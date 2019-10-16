﻿module EventManager
open CommonGenericFunctions
open GameEvents


type listenerCallback = AbstractGameEvent -> Result<string option,string>
type GameEventResult = (AbstractGameEvent * Result<string option,string>)

type EventListenerDictionary() =
    let mutable _listeners = Map.empty:Map<GameEventTypes,listenerCallback[]>

    member this.ContainsKey et = _listeners.ContainsKey et
    member this.Item et = _listeners.Item et
    member this.RegisterListener et callback = _listeners <- Map_AppendValueToArray _listeners et callback


type PendingEventsDictionary() =
    let mutable _pending = Array.empty<AbstractGameEvent>
    
    member this.ProcessEvents (listeners:EventListenerDictionary) = 
        let mutable processedEvents = Array.empty<GameEventResult>
        while (_pending.Length > 0) do
            processedEvents <- Array.append processedEvents (this.processEventBatch listeners)
        processedEvents

    member this.QueueEvent ge = 
        _pending <- Array.append _pending [|ge|]
        
    member private this.collectAndClearPending = 
        let p = _pending
        _pending <- Array.empty
        p

    member private this.processEventBatch (listeners:EventListenerDictionary) = 
        let processCallbacks (ge:AbstractGameEvent) = 
            match listeners.ContainsKey ge.GameEventType with
            | false -> [| (ge, Error "No listeners") |]
            | true -> listeners.Item ge.GameEventType |> Array.map (fun cb -> (ge,cb ge)) // Can't Parallel
        this.collectAndClearPending |> Array.collect (fun ge -> processCallbacks ge) // Can't Parallel

type EventManager() =
    let listeners = new EventListenerDictionary() 
    let pending = new PendingEventsDictionary()

    member this.ProcessEvents = pending.ProcessEvents listeners
    member this.QueueEvent ge = pending.QueueEvent ge
    member this.RegisterListener et callback = listeners.RegisterListener et callback
    
