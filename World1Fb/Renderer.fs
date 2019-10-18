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
   

let private RenderAll (enm:EntityManager) =
    let render (eid:uint32) (fs:FormComponent[]) =
        let f = fs |> Array.find (fun e -> e.EntityID = eid)
        DrawAt f.Symbol f.Location

    let fs = 
        FormComponent.Type
        |> enm.EntitiesWithComponent
        |> Array.map (fun eid -> enm.TryGetComponent<FormComponent> eid)
        |> Array.collect (fun fo -> fo |> Option.toArray)
    let ts =
        TerrainComponent.Type
        |> enm.EntitiesWithComponent
        |> Array.map (fun eid -> enm.TryGetComponent<TerrainComponent> eid)
        |> Array.collect (fun fo -> fo |> Option.toArray)

    ts |> Array.iter (fun t -> render t.EntityID fs)
    fs |> Array.filter (fun f-> not (ts |> Array.exists (fun t -> t.EntityID=f.EntityID))) |> Array.iter (fun f -> render f.EntityID fs)
    
    

let RenderFrame (enm:EntityManager) (fc:int) (fn:uint32) =
    System.Console.CursorVisible <- false 

    RenderAll enm

    System.Console.SetCursorPosition(MapWidth+1,MapHeight)
    System.Console.Write(sprintf "Frame count:%i - #:%i\n" fc fn)

