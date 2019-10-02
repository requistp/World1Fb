module Components
open FormComponent
open MovementComponent
open TerrainComponent
open System

type ComponentType = 
    | Form of FormComponent
    | Movement of MovementComponent
    | Terrain of TerrainComponent
    //member this.ComponentID = 
    //    match this with
    //    | Form _ -> ComponentID_Form
    //    | Movement _ -> ComponentID_Movement
    //    | Terrain _ -> ComponentID_Terrain

type ComponentChangeType = 
    | FormChange of FormComponent_Change
    | MovementChange of MovementComponent_Change
    | TerrainChange of TerrainComponent_Change
    member this.ComponentID = 
        match this with
        | FormChange _ -> ComponentID_Form
        | MovementChange _ -> ComponentID_Movement
        | TerrainChange _ -> ComponentID_Terrain


type EntityComponent = {
    EntityID : Guid
    Component : ComponentType
    } with
    member this.ComponentID = 
        match this.Component with
        | Form _ -> ComponentID_Form
        | Movement _ -> ComponentID_Movement
        | Terrain _ -> ComponentID_Terrain

type EntityComponentChange = 
    | ComponentChange of ComponentChangeType
    | EntityAddition of (Guid * EntityComponent list)
    | EntityRemoval of Guid
