module Renderer
open CommonGenericFunctions
open Components
open EntityComponentManager
open GameManager
open LocationTypes
open TerrainComponent

let DrawAt (c:char) (l:LocationDataInt) =
    System.Console.SetCursorPosition(int l.X, int l.Y)
    System.Console.Write(c)

let RenderTerrain (f:Frame) =
    let st = TimerStart

    for ecd in Entity.AllWithComponent f.EntityComponentData ComponentID_Terrain do
        let ct = ecd.Value |> List.find (fun x -> x.Component.ComponentID=ComponentID_Terrain)
        match ct.Component with
        | Terrain x -> ()//DrawAt x.Symbol x.Location
        | _ -> ()

    TimerEnd "render" st

//let RenderForms (f:Frame) =
//    for e in f.ECM.EntityIDsWithComponent (typeof<FormComponent>) do
//        let ac = f.ECM.GetComponent (typeof<FormComponent>) e
//        let fc = (ac :?> FormComponent)
//        DrawAt fc.Symbol fc.Location.Xint fc.Location.Yint

let RenderFrame (f:Frame) =
    RenderTerrain f
    //RenderForms f

let RenderFrames (fl:Frame list) =
    fl |> List.iter RenderFrame
