module Renderer
open CommonGenericFunctions
open Components
open EntityComponentManager
open GameManager
open LocationTypes
open TerrainComponent
open FormComponent

let DrawAt (c:char) location =
    System.Console.SetCursorPosition(int location.X, int location.Y)
    System.Console.Write(c)

let RenderTerrain f =
    let st = TimerStart
    for e in Entity.AllWithComponent f.ECD ComponentID_Terrain do
        match Entity.GetComponent f.ECD ComponentID_Terrain e with
        | Terrain x -> ()//DrawAt x.Symbol x.Location
        | _ -> ()
    TimerEnd "render" st

let RenderForms f =
    for e in Entity.AllWithComponent f.ECD ComponentID_Form do
        match Entity.GetComponent f.ECD ComponentID_Form e with
        | Form x -> ()//DrawAt x.Symbol x.Location
        | _ -> ()

let RenderFrame f =
    RenderTerrain f
    RenderForms f
    System.Console.SetCursorPosition(0,MapHeightInt+1)

let RenderFrames fl =
    fl |> List.iter RenderFrame
