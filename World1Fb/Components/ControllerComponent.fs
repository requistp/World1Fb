﻿module ControllerComponent
open AbstractComponent


type ControllerComponent(eid:uint32) = 
    inherit AbstractComponent(eid,Component_Controller)
    static member Type = Component_Controller    

    override this.Copy neweid = ControllerComponent(neweid) :> AbstractComponent
