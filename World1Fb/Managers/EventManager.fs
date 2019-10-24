module EventManager
open CommonGenericFunctions
open EntityDictionary
open EntityManager
open EventTypes


type GameEventCallback = NextEntityDictionary -> EventData_Generic -> Result<string option,string>

type GameEventResult = (EventData_Generic * Result<string option,string>)
    
type EventListenerDictionary() =
    let mutable _listeners = Map.empty:Map<GameEventTypes,GameEventCallback[]>

    member this.ContainsKey et = _listeners.ContainsKey et
    member this.Item et = _listeners.Item et
    member this.RegisterListener et callback = _listeners <- Map_AppendValueToArray _listeners et callback


type PendingEventsDictionary() =
    let mutable _pending = Array.empty<EventData_Generic>
    
    member this.ProcessEvents (listeners:EventListenerDictionary) (next:NextEntityDictionary) = 
        //let mutable processedEvents = Array.empty<GameEventResult>
        //while (_pending.Length > 0) do
        //    processedEvents <- Array.append processedEvents (this.processEventBatch listeners next)
        //processedEvents
        while (_pending.Length > 0) do
            this.processEventBatch listeners next
        
    member this.QueueEvent ge = 
        _pending <- Array.append _pending [|ge|]
        
    member private this.collectAndClearPending = 
        let p = _pending
        _pending <- Array.empty
        p

    member private this.processEventBatch (listeners:EventListenerDictionary) (next:NextEntityDictionary) = 
        let processCallbacks (ge:EventData_Generic) = 
            match listeners.ContainsKey ge.GameEventType with 
            | false -> () //[| (ge, Error "No listeners") |]
            | true -> listeners.Item ge.GameEventType 
                      |> Array.iter (fun cb -> printfn "%A" ge.ToString ; (cb next ge) |> ignore) // Can't Parallel

        this.collectAndClearPending |> Array.iter (fun ge -> processCallbacks ge) // Can't Parallel


type EventManager(enm:EntityManager) =
    let listeners = new EventListenerDictionary() 
    let pending = new PendingEventsDictionary()
    
    member this.ProcessEvents = pending.ProcessEvents listeners enm.NextEntityDictionary
    member this.QueueEvent ge = pending.QueueEvent ge
    member this.RegisterListener et callback = listeners.RegisterListener et callback
    
    (*member private this.processEventBatch (listeners:EventListenerDictionary) (next:NextEntityDictionary) = 
        let processCallbacks (ge:EventData_Generic) = 
            match listeners.ContainsKey ge.GameEventType with 
            | false -> [| (ge, Error "No listeners") |]
            | true -> listeners.Item ge.GameEventType 
                      |> Array.map (fun cb -> (ge,cb next ge)) // Can't Parallel
        this.collectAndClearPending |> Array.collect (fun ge -> processCallbacks ge) // Can't Parallel
*)