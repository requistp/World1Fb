module System_Abstract
open ChangeLogManager
open Components
open EntityComponentManager

[<AbstractClass>]
type AbstractSystem(isActive:bool, requireInitialize:bool) =
    let mutable _isInitialized = false

    member this.IsActive = isActive
    member this.RequireInitialize = requireInitialize
    member this.IsInitialized
        with get () = (_isInitialized || not requireInitialize)
        and internal set (value) = _isInitialized <- value

    abstract member Initialize: ChangeLog list
    abstract member Update: EntityComponentManager -> ChangeLog
