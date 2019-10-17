module ControllerComponent
open AbstractComponent


type ControllerComponent(eid:uint32) = 
    inherit AbstractComponent(eid,Comp_Controller)

    static member Type = Comp_Controller    
    
