module ControllerComponent
open AbstractComponent


type ControllerComponent(eid:uint32) = 
    inherit AbstractComponent(eid,Controller)

