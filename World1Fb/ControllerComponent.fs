module ControllerComponent
open AbstractComponent


type ControllerComponent(eid:uint32) = 
    inherit AbstractComponent(eid,Controller)

    override this.NewWithEID eid = ControllerComponent(eid) :> AbstractComponent

    new() = ControllerComponent(0u)

