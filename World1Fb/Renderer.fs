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


//let private RenderTerrain (em:EntityManager) =
//    //let st = Timer.Start
//    for eid in Terrain |> em.GetAllWithComponent do
//        let c = (eid|>em.GetComponent Terrain) :?> TerrainComponent
//        DrawAt c.Symbol c.Location
//    //System.Console.SetCursorPosition(0,MapHeightInt+1)
//    //Timer.End "render Terrain" st
//let private RenderForms (em:EntityManager) =
//    //let st = Timer.Start
//    for eid in Form |> em.GetAllWithComponent do
//        let c = (eid|>em.GetComponent Form) :?> FormComponent
//        DrawAt c.Symbol c.Location
//    //System.Console.SetCursorPosition(0,MapHeightInt+5)
//    //Timer.End "render Forms" st

let private RenderAll (enm:EntityManager) =
    let render (f:FormComponent) =
        DrawAt f.Symbol f.Location
    let renderMap (m:Map<uint32,AbstractComponent option[]>) =
        m |> 
        Map.iter (fun k v -> v |> Array.filter (fun aco -> aco.IsSome && aco.Value.ComponentType = Form) |> Array.iter (fun f -> render (f.Value :?> FormComponent)))
    let t,nt = 
        enm.GetAllWithComponent Form 
        |> Array.fold (fun (m:Map<uint32,AbstractComponent option[]>) eid -> m.Add(eid,enm.TryGetComponents eid [| Form; Terrain |]) ) Map.empty
        |> Map.partition (fun k v -> v |> Array.exists (fun aco -> aco.IsSome && aco.Value.ComponentType = Terrain))
    renderMap t
    renderMap nt
    

let RenderFrame (enm:EntityManager) fn =
    System.Console.CursorVisible <- false 

    RenderAll enm

    System.Console.SetCursorPosition(MapWidth+1,MapHeight)
    System.Console.Write(sprintf "Frame #%i\n" fn)

