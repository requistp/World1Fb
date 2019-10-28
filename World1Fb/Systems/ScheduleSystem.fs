module ScheduleSystem
open AbstractComponent
open AbstractSystem
open CalendarTimings
open CommonGenericFunctions
open EntityManager
open EventTypes
open GameManager
open ScheduleAgent


type ScheduleSystem(game:Game, isActive:bool) =
    inherit AbstractSystem(isActive) 
    let schedule = new ScheduleAgent(game.Round)

    member _.List = schedule.List

    member private this.onScheduleEvent (enm:EntityManager) (ge:EventData_Generic) =
        let e = (ge :?> EventData_ScheduleEvent).ScheduledEvent
        let result = sprintf "Scheduled for EntityID:%i. Freq:%i" e.EventData.EntityID e.Frequency
        schedule.Add e
        Ok (Some result)

    member private this.ProcessScheduledEvents =
        let executeAndReschedule (se:ScheduledEvent) =
            schedule.Reschedule se
            game.EventManager.QueueEvent se.EventData
        schedule.Get
        |> Array.filter (fun se -> game.EntityManager.Exists se.EventData.EntityID)
        |> Array.Parallel.iter (fun se -> executeAndReschedule se)

    override this.Initialize = 
        game.EventManager.RegisterListener ScheduleEvent this.onScheduleEvent
        base.SetToInitialized

    override this.Update = 
        this.ProcessScheduledEvents 
        ()

