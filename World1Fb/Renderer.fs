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
    for ecd in Entity.AllWithComponent f.ECD ComponentID_Terrain do
        match (ecd.Value |> List.find (fun x -> x.ComponentID=ComponentID_Terrain)).Component with
        | Terrain x -> DrawAt x.Symbol x.Location
        | _ -> ()

let RenderForms f =
    for ecd in Entity.AllWithComponent f.ECD ComponentID_Form do
        match (ecd.Value |> List.find (fun x -> x.ComponentID=ComponentID_Form)).Component with
        | Form x -> DrawAt x.Symbol x.Location
        | _ -> ()        

let RenderFrame f =
    RenderTerrain f
    RenderForms f
    System.Console.SetCursorPosition(0,MapHeightInt+1)

let RenderFrames fl =
    fl |> List.iter RenderFrame
