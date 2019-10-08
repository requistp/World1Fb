module ControllerComponent
open AbstractComponent

//[<Literal>]
//let ComponentID_Controller = 4uy

type ControllerComponent() = 
    inherit AbstractComponent(Controller)
