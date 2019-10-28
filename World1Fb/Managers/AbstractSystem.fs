module AbstractSystem


[<AbstractClass>]
type AbstractSystem(isActive:bool) =
    let mutable _isInitialized = false

    member this.IsActive = isActive
    member this.IsInitialized = _isInitialized
    member this.SetToInitialized = _isInitialized <- true

    abstract member Initialize : unit
    default this.Initialize = this.SetToInitialized

    abstract member Update : unit

    member this.Abstract = this :> AbstractSystem

