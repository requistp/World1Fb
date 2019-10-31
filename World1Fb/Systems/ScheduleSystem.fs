module ScheduleSystem
open AbstractSystem
open agentSchedule
open EventTypes
open GameManager


type ScheduleSystem(game:Game, isActive:bool) =
    inherit AbstractSystem(isActive) 
    let enm = game.EntityManager
    let evm = game.EventManager    
    let schedule = new agentSchedule(game.Round)

    //member _.List = schedule.List

    member private me.onScheduleEvent (ge:EventData_Generic) =
        let e = (ge :?> EventData_ScheduleEvent).ScheduledEvent
        let result = sprintf "Scheduled for EntityID:%i. Freq:%i" e.EventData.EntityID e.Frequency
        schedule.Add e
        Ok (Some result)

    member private me.ProcessScheduledEvents =
        let executeAndReschedule (se:ScheduledEvent) =
            schedule.Reschedule se
            evm.QueueEvent se.EventData
        schedule.Get
        |> Array.filter (fun se -> enm.Exists se.EventData.EntityID) // This makes sure the entity for the scheduled event still exists
        |> Array.Parallel.iter (fun se -> executeAndReschedule se)

    override me.Initialize = 
        evm.RegisterListener "ScheduleSystem" ScheduleEvent me.onScheduleEvent
        base.SetToInitialized

    override me.Update = 
        me.ProcessScheduledEvents 
        ()

