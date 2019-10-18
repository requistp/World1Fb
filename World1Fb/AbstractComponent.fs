﻿module AbstractComponent


type ComponentTypes =
    | Component_Eating
    | Component_Food
    | Component_Form
    | Component_Controller
    | Component_Movement
    | Component_Terrain


[<AbstractClass>]
type AbstractComponent(eid:uint32, componentType:ComponentTypes) =
    member this.ComponentType = componentType
    member this.EntityID = eid


