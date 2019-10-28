module ScheduleSystem
open AbstractComponent
open AbstractSystem
open CalendarTimings
open CommonGenericFunctions
open EntityManager
open EventTypes
open GameManager


type ScheduleSystem(game:Game, isActive:bool) =
    inherit AbstractSystem(isActive) 

    let mutable _schedule = Map.empty<uint32,ScheduledEvent[]>

    member private this.AddEvent (se:ScheduledEvent) =
        let offset = uint32 (TimingOffset (int se.Frequency))
        let result = sprintf "Scheduled for EntityID:%i. Freq:%i. Offset:%i. First Round:%i" se.EventData.EntityID se.Frequency offset (game.Round+offset)
        _schedule <- Map_AppendValueToArray _schedule (game.Round+offset) se
        Ok (Some result)

    member private this.onScheduleEvent (enm:EntityManager) (ge:EventData_Generic) =
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

