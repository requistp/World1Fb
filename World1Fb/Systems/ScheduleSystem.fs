module ScheduleSystem
open AbstractComponent
open AbstractSystem
open CalendarTimings
open CommonGenericFunctions
open EntityDictionary
open GameEvents
open GameManager


type ScheduleSystem(game:Game, isActive:bool) =
    inherit AbstractSystem(isActive) 

    let mutable _schedule = Map.empty<uint32,ScheduledEvent[]>

    member private this.AddEvent (se:ScheduledEvent) =
        _schedule <- Map_AppendValueToArray _schedule (game.Round + uint32 (TimingOffset (int se.Frequency))) se
        Ok None

    member private this.onScheduleEvent (next:NextEntityDictionary) (ge:EventData_Generic) =
        this.AddEvent ((ge :?> EventData_ScheduleEvent).ScheduledEvent)

    member private this.ProcessScheduledEvents =
        let executeAndReschedule (se:ScheduledEvent) =
            _schedule <- Map_AppendValueToArray (_schedule.Remove se.EventData.EntityID) (game.Round + se.Frequency) se //Remove current and Add future 
            game.EventManager.QueueEvent se.EventData
        let e,ne = _schedule.Item(game.Round) |> Array.partition (fun se -> game.EntityManager.Exists se.EventData.EntityID)
        ne |> Array.iter (fun se -> _schedule <- _schedule.Remove se.EventData.EntityID)
        e |> Array.iter (fun se -> executeAndReschedule se)

    override this.Initialize = 
        game.EventManager.RegisterListener ScheduleEvent this.onScheduleEvent
        base.SetToInitialized

    override this.Update = 
        match _schedule.ContainsKey(game.Round) with
        | false -> ()
        | true -> this.ProcessScheduledEvents 
