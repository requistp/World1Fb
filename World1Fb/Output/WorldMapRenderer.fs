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

    member me.Update (enm:EntityManager) (round:RoundNumber option) = 
        Console.CursorVisible <- false
        Console.Title <- "World Map"
        
        let allForms = enm.GetLocationMap None

        let rangeY = 
            [|(snd _windowLocation)..(snd _windowLocation + viewSizeY - 1)|]

        let rangeX = 
            [|(fst _windowLocation)..(fst _windowLocation + viewSizeX - 1)|]
        
        for y in rangeY do
            for x in rangeX do
                let selectForm (fds:FormComponent[]) = 
                    match fds.Length with
                    | 1 -> fds.[0]
                    | _ ->
                        match fds |> Array.tryFind (fun c -> (EntityExt.TryGetComponent enm round ControllerComponent c.EntityID).IsSome) with
                        | Some f -> f
                        | None ->
                            (fds |> Array.sortBy (fun c -> (EntityExt.TryGetComponent enm round TerrainComponent c.EntityID).IsSome)).[0]
                
                let fs = allForms.Item({ X = x; Y = y; Z = 0 })

                match fs.Length with
                | 0 -> ()
                | _ ->
                    let fd = selectForm fs
                    System.Console.SetCursorPosition(x - fst _windowLocation, y - snd _windowLocation)
                    System.Console.Write(fd.Symbol)
        
    member me.UpdateEntity (enm:EntityManager) (entityID:EntityID) = 
        Console.CursorVisible <- false
        Console.Title <- "Entity Viewer"
        Console.Clear()

        let centerX = 30
        let centerY = 10

        let (Vision v) = enm.GetComponent None VisionComponent entityID
        let (Form f) = enm.GetComponent None FormComponent entityID

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
                    match v.VisibleLocations |> Array.contains location with
                    | false -> ColoredConsole.Console.DrawDarkGray
                    | true -> ColoredConsole.Console.DrawWhite
                let forms = 
                    //let es = enm.GetEntityMap (Some round) 
                    location
                    |> enm.GetFormsAtLocation (Some round)
                    |> Array.sortByDescending (fun f -> f.ID)
                    |> Array.head
                    //|> EntityExt.GetHistory_Locations enm (Some round)
                    //|> Array.Parallel.map (fun e -> (e|>EntityExt.GetComponent2 es FormComponentID).ToForm)
                System.Console.SetCursorPosition(drawX,drawY)
                //drawCall forms.[0].Symbol
                drawCall forms.Symbol
            )



