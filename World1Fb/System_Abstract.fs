module System_Abstract
open ChangeLogManager
open Components
open EntityComponentManager

[<AbstractClass>]
type AbstractSystem(isActive:bool, requireInitialize:bool) =
    let mutable _isInitialized = false

    member this.IsActive = isActive
    member this.RequireInitialize = requireInitialize
    member this.IsInitialized = (_isInitialized || not requireInitialize)

    member internal this.SetToInitialized = _isInitialized <- true
    abstract member Initialize: EntityComponentChange list
    abstract member Update: EntityComponentData -> EntityComponentChange list
