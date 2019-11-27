module WorldMapRenderer
open CommonGenericFunctions
open Component
open ComponentEnums
open FormComponent
open EntityManager
open EntityExtensions
open LocationTypes
open System
//open ColoredConsole


type WorldMapRenderer() =
    let mutable _windowLocation = (0,0)
    let viewSizeX = Math.Clamp(40,5,MapWidth)
    let viewSizeY = Math.Clamp(20,5,MapHeight)
    
    member me.MoveWindow (k:ConsoleKey) =
        match k with 
        | ConsoleKey.UpArrow -> _windowLocation <- (fst _windowLocation, Math.Clamp(snd _windowLocation - 1, 0, MapHeight-viewSizeY))
        | ConsoleKey.DownArrow -> _windowLocation <- (fst _windowLocation, Math.Clamp(snd _windowLocation + 1, 0, MapHeight-viewSizeY))
        | ConsoleKey.LeftArrow -> _windowLocation <- (Math.Clamp(fst _windowLocation - 1, 0, MapWidth-viewSizeX), snd _windowLocation)
        | ConsoleKey.RightArrow -> _windowLocation <- (Math.Clamp(fst _windowLocation + 1, 0, MapWidth-viewSizeX), snd _windowLocation)
        | _ -> ()

    member me.UpdateWorld (enm:EntityManager) = 
        Console.CursorVisible <- false
        Console.Title <- "World Map"
        
        let allForms = enm.GetLocationMap 

        let rangeY = 
            [|(snd _windowLocation)..(snd _windowLocation + viewSizeY - 1)|]

        let rangeX = 
            [|(fst _windowLocation)..(fst _windowLocation + viewSizeX - 1)|]
        
        for y in rangeY do
            for x in rangeX do
                let selectForm (fds:FormComponent[]) = 
                    fds
                    |> Array.sortByDescending (fun f -> f.ID)
                    |> Array.head
                    //match fds.Length with
                    //| 1 -> fds.[0]
                    //| _ ->
                    //    match fds |> Array.tryFind (fun c -> (EntityExt.TryGetComponent enm ControllerComponent c.EntityID).IsSome) with
                    //    | Some f -> f
                    //    | None ->
                    //        (fds |> Array.sortBy (fun c -> (EntityExt.TryGetComponent enm TerrainComponent c.EntityID).IsSome)).[0]
                
                let formsAtLocation = allForms.Item(LocationDataInt(x,y,0))

                match formsAtLocation.Length with
                | 0 -> ()
                | _ ->
                    let fd = selectForm formsAtLocation
                    System.Console.SetCursorPosition(x - fst _windowLocation, y - snd _windowLocation)
                    System.Console.Write(fd.Symbol)
        
    member me.UpdateEntity (enm:EntityManager) (entityID:EntityID) = 
        Console.CursorVisible <- false
        Console.Title <- "Entity Viewer"
        Console.Clear()

        let centerX = 30
        let centerY = 10

        let (Vision v) = enm.GetComponent VisionComponentType entityID
        let (Form f) = enm.GetComponent ComponentEnums.FormComponentType entityID

        let addX = centerX - f.Location.X
        let addY = centerY - f.Location.Y
        
        v.ViewedHistory
        |> Map.iter (fun location round -> 
            let drawX = location.X + addX
            let drawY = location.Y + addY
            match drawX >= 0 && drawY >= 0 with
            | false -> ()
            | true -> 
                let drawCall = 
                    match (v.VisibleLocations.ContainsKey location) with
                    | false -> ColoredConsole.Console.DrawDarkGray
                    | true -> ColoredConsole.Console.DrawWhite
                let formChar = 
                    (
                    match (v.VisibleLocations.ContainsKey location) with
                    | false -> 
                        v.ViewedHistory.Item location
                        |> Array.sortByDescending (fun f -> f.ID)
                        |> Array.head
                    | true -> 
                        v.ViewedHistory.Item location
                        |> Array.sortByDescending (fun f -> f.ID)
                        |> Array.head
                    ).Symbol
                let countAsChar = // Useful for debugging
                    match (v.VisibleLocations.ContainsKey location) with
                    | false -> 
                        (v.ViewedHistory.Item location).Length.ToString().ToCharArray().[0]
                    | true -> 
                        (v.ViewedHistory.Item location
                        |> Array.sortByDescending (fun f -> f.ID)
                        |> Array.head).Symbol
                System.Console.SetCursorPosition(drawX,drawY)
                drawCall formChar
            )

