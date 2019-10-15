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
    let render (f:FormComponent) =
        DrawAt f.Symbol f.Location
    let renderMap (m:Map<uint32,AbstractComponent option[]>) =
        let renderForms (aco:AbstractComponent option[]) = 
            aco
            |> Array.filter (fun aco -> aco.IsSome && aco.Value.ComponentType = Form) 
            |> Array.iter (fun f -> render (f.Value :?> FormComponent))
        m |> Map.iter (fun k v -> renderForms v)
    let t,nt = 
        enm.EntitiesWithComponent Form 
        |> Array.fold (fun (m:Map<uint32,AbstractComponent option[]>) eid -> m.Add(eid,enm.TryGetComponents eid [| Form; Terrain |]) ) Map.empty
        |> Map.partition (fun k v -> v |> Array.exists (fun aco -> aco.IsSome && aco.Value.ComponentType = Terrain))
    renderMap t
    renderMap nt
    

let RenderFrame (enm:EntityManager) (fc:int) (fn:uint32) =
    System.Console.CursorVisible <- false 

    RenderAll enm

    System.Console.SetCursorPosition(MapWidth+1,MapHeight)
    System.Console.Write(sprintf "Frame count:%i - #:%i\n" fc fn)

