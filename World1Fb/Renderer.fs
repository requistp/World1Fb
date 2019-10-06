module Renderer
open CommonGenericFunctions
open Components
open EntityComponentManager
open GameManager
open LocationTypes
open TerrainComponent
open FormComponent

let private DrawAt (c:char) location =
    System.Console.SetCursorPosition(int location.X, int location.Y)
    System.Console.Write(c)

let private RenderTerrain f =
    //let st = Timer.Start
    for e in Entity.AllWithComponent f.ECD ComponentID_Terrain do
        match Entity.GetComponent f.ECD ComponentID_Terrain e with
        | Terrain x -> DrawAt x.Symbol x.Location
        | _ -> ()
    //System.Console.SetCursorPosition(0,MapHeightInt+1)
    //Timer.End "render Terrain" st

let private RenderForms f =
    //let st = Timer.Start
    for e in Entity.AllWithComponent f.ECD ComponentID_Form do
        match Entity.GetComponent f.ECD ComponentID_Form e with
        | Form x -> DrawAt x.Symbol x.Location
        | _ -> ()
    //System.Console.SetCursorPosition(0,MapHeightInt+5)
    //Timer.End "render Forms" st

let RenderFrame f =
    System.Console.CursorVisible <- false 
    RenderTerrain f
    RenderForms f

    System.Console.SetCursorPosition(MapWidthInt + 1,MapHeightInt)
    System.Console.Write(sprintf "Frame #%i" f.Number)

let RenderFrames fl =
    fl |> List.iter RenderFrame
