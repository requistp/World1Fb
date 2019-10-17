module AbstractSystem
open EntityDictionary
open EventManager
open GameEvents


type SystemTypes =
    | Sys_Eating
    | Sys_Food
    | Sys_Form
    | Sys_Movement
    | Sys_Terrain


[<AbstractClass>]
type AbstractSystem(sysType:SystemTypes, isActive:bool) =
    let mutable _isInitialized = false

    member this.IsActive = isActive
    member this.IsInitialized = _isInitialized
    member this.SetToInitialized = _isInitialized <- true
    member this.SystemType = sysType

    abstract member Initialize : unit
    default this.Initialize = this.SetToInitialized

    abstract member Update : unit
    default this.Update = ()
