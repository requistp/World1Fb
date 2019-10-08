module Renderer
open CommonGenericFunctions
open AbstractComponent
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
    for eid in Terrain |> Entity.AllWithComponent f.ECD do
        let c = (eid |> Entity.GetComponent f.ECD Terrain) :?> TerrainComponent
        DrawAt c.Symbol c.Location
    //System.Console.SetCursorPosition(0,MapHeightInt+1)
    //Timer.End "render Terrain" st

let private RenderForms f =
    //let st = Timer.Start
    for eid in Form |> Entity.AllWithComponent f.ECD do
        let c = (eid |> Entity.GetComponent f.ECD Form) :?> FormComponent
        DrawAt c.Symbol c.Location
    //System.Console.SetCursorPosition(0,MapHeightInt+5)
    //Timer.End "render Forms" st

let RenderFrame f =
    System.Console.CursorVisible <- false 
    RenderTerrain f
    RenderForms f

    System.Console.SetCursorPosition(MapWidth+1,MapHeight)
    System.Console.Write(sprintf "Frame #%i\n" f.Number)

let RenderFrames fl =
    fl |> List.iter RenderFrame
