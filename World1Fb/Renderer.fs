module Renderer
open CommonGenericFunctions
open AbstractComponent
open EntityManager
open GameManager
open LocationTypes
open TerrainComponent
open FormComponent


let private DrawAt (c:char) location =
    System.Console.SetCursorPosition(int location.X, int location.Y)
    System.Console.Write(c)


let private RenderTerrain (em:EntityManager) =
    //let st = Timer.Start
    for eid in Terrain |> em.GetAllWithComponent do
        let c = (eid|>em.GetComponent Terrain) :?> TerrainComponent
        DrawAt c.Symbol c.Location
    //System.Console.SetCursorPosition(0,MapHeightInt+1)
    //Timer.End "render Terrain" st


let private RenderForms (em:EntityManager) =
    //let st = Timer.Start
    for eid in Form |> em.GetAllWithComponent do
        let c = (eid|>em.GetComponent Form) :?> FormComponent
        DrawAt c.Symbol c.Location
    //System.Console.SetCursorPosition(0,MapHeightInt+5)
    //Timer.End "render Forms" st


let RenderFrame (em:EntityManager) fn =
    System.Console.CursorVisible <- false 
    RenderTerrain em
    RenderForms em

    System.Console.SetCursorPosition(MapWidth+1,MapHeight)
    System.Console.Write(sprintf "Frame #%i\n" fn)

