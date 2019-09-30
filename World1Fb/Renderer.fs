module Renderer
open CommonGenericFunctions
open Components
open EntityComponentManager
open GameManager
open LocationTypes
open TerrainComponent
open FormComponent

let DrawAt (c:char) (l:LocationDataInt) =
    System.Console.SetCursorPosition(int l.X, int l.Y)
    System.Console.Write(c)

let RenderTerrain (f:Frame) =
    for ecd in Entity.AllWithComponent f.EntityComponentData ComponentID_Terrain do
        match (ecd.Value |> List.find (fun x -> x.Component.ComponentID=ComponentID_Terrain)).Component with
        | Terrain x -> DrawAt x.Symbol x.Location
        | _ -> ()

let RenderForms (f:Frame) =
    for ecd in Entity.AllWithComponent f.EntityComponentData ComponentID_Form do
        match (ecd.Value |> List.find (fun x -> x.Component.ComponentID=ComponentID_Form)).Component with
        | Form x -> DrawAt x.Symbol x.Location
        | _ -> ()        

let RenderFrame (f:Frame) =
    RenderTerrain f
    RenderForms f
    System.Console.SetCursorPosition(0,MapHeightInt+1)

let RenderFrames (fl:Frame list) =
    fl |> List.iter RenderFrame
