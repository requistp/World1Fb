module EventTypes
open AbstractComponent
open MovementComponent

type GameEventTypes =
    | Action_Eat
    | Action_Movement
    | CreateEntity
    | Eaten
    | Food_AllEaten
    | Kill_AllEaten
    | Metabolize
    | Movement
    | PlantRegrowth
    | PlantReproduce
    | ScheduleEvent
    | Starving


type EventData_Generic(gameEventType:GameEventTypes, eid:uint32) =
    member _.EntityID = eid
    member _.GameEventType = gameEventType
    member this.ToString = sprintf "%s (#%i)" (this.GameEventType.ToString()) eid
    new (gameEventType:GameEventTypes) = EventData_Generic(gameEventType, 0u)

type ScheduledEvent(ed:EventData_Generic, frequency:uint32) =
    member _.EventData = ed
    member _.Frequency = frequency
    member this.ToString = sprintf "EventData:%s. Frequency:%i" (ed.ToString) frequency

//-------------------------------------------------------------------------------------------------------------------------------------------

type EventData_Action_Movement(eid:uint32, direction:MovementDirection) =
    inherit EventData_Generic(Action_Movement, eid)
    member _.Direction = direction
    member this.ToString = base.ToString + (sprintf " Direction:%s" (direction.ToString()))

type EventData_CreateEntity(eid: uint32, cts:AbstractComponent[]) =
    inherit EventData_Generic(CreateEntity, eid)
    member _.Components = cts         
    member this.ToString = base.ToString + (sprintf " Components:%i" (cts.Length))

type EventData_Eaten(eaterID:uint32, eateeID:uint32, quantity:int) =
    inherit EventData_Generic(Eaten, eaterID)
    member _.EateeID = eateeID
    member _.Quantity = quantity
    member this.ToString = base.ToString + (sprintf " EateeID:%i. Quantity:%i" eateeID quantity)

type EventData_Movement(eid:uint32, direction:MovementDirection) =
    inherit EventData_Generic(Movement,eid)
    member _.Direction = direction
    member this.ToString = base.ToString + (sprintf " Direction:%s" (direction.ToString()))
    
type EventData_ScheduleEvent(eid:uint32, se:ScheduledEvent) =
    inherit EventData_Generic(ScheduleEvent,eid)
    member _.ScheduledEvent = se
    member this.ToString = sprintf "%s - %s" (this.GameEventType.ToString()) (se.ToString)




//[<AbstractClass>]
//type Event_Action_Eat(eid:uint32) =
//    inherit GenericGameEventData(Action_Eat)
//    member _.EntityID = eid
//    override this.Print = sprintf "%s - Entity:%i" (this.GameEventType.ToString()) eid

//type Event_Food_AllEaten(eid:uint32) =
//    inherit GenericGameEventData(Food_AllEaten)
//    member _.EntityID = eid
//    override this.Print = sprintf "%s - Entity:%i" (this.GameEventType.ToString()) eid

//type Event_Kill_AllEaten(eid:uint32) =
//    inherit GenericGameEventData(Kill_AllEaten)
//    member _.EntityID = eid           
//    override this.Print = sprintf "%s - Entity:%i" (this.GameEventType.ToString()) eid

//type Event_Metabolize(eid:uint32) =
//    inherit GenericGameEventData(Metabolize)
//    member _.EntityID = eid           
//    override this.Print = sprintf "%s - Entity:%i" (this.GameEventType.ToString()) eid

//type Event_PlantRegrowth(eid:uint32) =
//    inherit GenericGameEventData(PlantRegrowth)
//    member _.EntityID = eid
//    override this.Print = sprintf "%s - Entity:%i" (this.GameEventType.ToString()) eid

//type Event_Starving(eid:uint32) =
//    inherit GenericGameEventData(Starving)
//    member _.EntityID = eid           
//    override this.Print = sprintf "%s - Entity:%i" (this.GameEventType.ToString()) eid
         
         
   