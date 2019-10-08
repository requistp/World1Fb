module EventManager
open System
open MovementComponent

let GameEventID_KeyPressed = 1
let GameEventID_KeyPressed_Movement = 2
let GameEventID_Movement = 3

type GameEvent = 
    | KeyPressed of ConsoleKey
    | KeyPressed_Movement of MovementDirection
    | Movement of MovementComponent_Change
    member this.GameEventID = 
        match this with
        | KeyPressed _ -> GameEventID_KeyPressed
        | KeyPressed_Movement _ -> GameEventID_KeyPressed_Movement
        | Movement _ -> GameEventID_Movement

type listenerCallback = GameEvent -> unit

type EventManager() =
    let mutable _listeners = Map.empty:Map<int,listenerCallback[]> 
    let mutable _pendingEvents = Array.empty<GameEvent>

    let processCallbacks (e:GameEvent) = 
        match _listeners.ContainsKey(e.GameEventID) with
        | false -> ()
        | true -> _listeners.Item(e.GameEventID) |> Array.iter (fun cb -> cb e)

    member _.ProcessEvents = 
        let mutable processedEvents = Array.empty<GameEvent>

        while _pendingEvents.Length > 0 do
            processedEvents <- Array.append processedEvents _pendingEvents
            _pendingEvents <- Array.empty<GameEvent>
            processedEvents |> Array.iter (fun e -> processCallbacks e)
            //printfn "processed events = %i" processedEvents.Length
            //printfn "pending events   = %i" _pendingEvents.Length
        processedEvents

    member _.QueueEvent e = 
        _pendingEvents <- Array.append _pendingEvents [|e|]

    member _.RegisterListener eid callback = 
        match _listeners.ContainsKey(eid) with
        | false -> _listeners <- _listeners.Add(eid,[|callback|])
        | true -> let l = Array.append (_listeners.Item(eid)) [|callback|]
                  _listeners <- _listeners.Remove(eid).Add(eid,l)


        //match e with 
        //| Test1 -> printf "Queue 1"
        //| Test2 i -> printfn "Queue 2 %i" i
        //| Test3 s -> printf "Queue 3 %s" s
        //| KeyPressed k -> printfn "handle %A" k

