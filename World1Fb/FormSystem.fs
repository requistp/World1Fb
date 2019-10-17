module FormSystem
open AbstractSystem
open FormComponent
open GameEvents
open GameManager
open LocationTypes
open SystemManager


type FormSystem(game:Game, isActive:bool) =
    inherit AbstractSystem(Sys_Form, isActive) 

