module WorldMapRenderer
open Component
open EntityManager
open LocationTypes
open System


type WorldMapRenderer() =
    let mutable _windowLocation = (0,0)
    let viewSizeX = Math.Clamp(40,10,MapWidth)
    let viewSizeY = Math.Clamp(20,10,MapHeight)
    
    member this.MoveWindow (k:ConsoleKey) =
        match k with 
        | ConsoleKey.UpArrow -> _windowLocation <- (fst _windowLocation, Math.Clamp(snd _windowLocation - 1, 0, MapHeight-viewSizeY))
        | ConsoleKey.DownArrow -> _windowLocation <- (fst _windowLocation, Math.Clamp(snd _windowLocation + 1, 0, MapHeight-viewSizeY))
        | ConsoleKey.LeftArrow -> _windowLocation <- (Math.Clamp(fst _windowLocation - 1, 0, MapWidth-viewSizeX), snd _windowLocation)
        | ConsoleKey.RightArrow -> _windowLocation <- (Math.Clamp(fst _windowLocation + 1, 0, MapWidth-viewSizeX), snd _windowLocation)
        | _ -> ()

    member this.Update (enm:EntityManager) = 
        //Console.Clear()
        //Console.SetBufferSize(MapWidth,MapHeight)
        //Console.SetBufferSize(Math.Max(MapWidth,Console.WindowWidth), Math.Max(MapHeight,Console.WindowHeight))
        Console.CursorVisible <- false
        Console.Title <- "World Map"
        
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
                        match fds |> Array.tryFind (fun c -> (enm.TryGetComponent ControllerComponent.ID c.EntityID).IsSome) with
                        | Some f -> f
                        | None ->
                            (fds |> Array.sortBy (fun c -> (enm.TryGetComponent TerrainComponent.ID c.EntityID).IsSome)).[0]
                let fs = 
                    enm.GetEntitiesAtLocation { X = x; Y = y; Z = 0 } 
                    |> enm.TryGetComponentForEntities FormComponent.ID
                    |> Array.map (fun c -> c.ToForm)

                match fs.Length with
                | 0 -> ()
                | _ ->
                    let fd = selectForm fs
                    System.Console.SetCursorPosition(x - fst _windowLocation, y - snd _windowLocation)
                    System.Console.Write(fd.Symbol)

        



    //let mutable _windows = Map.empty<string,Window>
    //let mutable _current = (fst wds.[0])
       
    //let setContent (wds:(string * string)[]) =
    //       _windows <-
    //           wds
    //           |> Array.fold (fun ws w -> ws.Add((fst w),Window(w))) Map.empty<string,Window>

    //do 
    //    System.Console.CursorVisible <- false
    //    setContent wds
    //    Console.Title <- _current

    //member this.Count = _windows.Count

    //member this.Display (s:string) =
    //    match _current with
    //    | "Game Events List" -> Console.Clear()
    //    | _ -> ()

    //    _windows.Item(_current).Display
    //    Console.WriteLine s

    //member this.SetContent (wds:(string * string)[]) (display:bool) =
    //    async
    //        {
    //            setContent wds
    //            if display then this.Display ""
    //        }
    //member this.SetDisplay (uniqueName:string) =
    //    match uniqueName <> _current && _windows.ContainsKey(uniqueName) with
    //    | false -> ()
    //    | true -> 
    //        _current <- uniqueName
    //        Console.Title <- uniqueName
    //        Console.Clear()
    //        this.Display ""
        
