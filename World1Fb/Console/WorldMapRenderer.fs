module WorldMapRenderer
open AbstractComponent
open EntityDictionary
open EntityManager
open FormComponent
open LocationTypes
open System
open TerrainComponent


type WorldMapRenderer() =
    let mutable _windowLocation = (0uy,0uy)
    let viewSizeX = 40uy
    let viewSizeY = 20uy
    
    member this.MoveWindow (k:ConsoleKey) =
        match k with 
        | ConsoleKey.UpArrow -> _windowLocation <- (fst _windowLocation, Math.Clamp(snd _windowLocation - 1uy, 0uy, MapHeight-viewSizeY))
        | ConsoleKey.DownArrow -> _windowLocation <- (fst _windowLocation, Math.Clamp(snd _windowLocation + 1uy, 0uy, MapHeight-viewSizeY))
        | ConsoleKey.LeftArrow -> _windowLocation <- (Math.Clamp(fst _windowLocation - 1uy, 0uy, MapWidth-viewSizeX), snd _windowLocation)
        | ConsoleKey.RightArrow -> _windowLocation <- (Math.Clamp(fst _windowLocation + 1uy, 0uy, MapWidth-viewSizeX), snd _windowLocation)
        | _ -> ()

    member this.Update (enm:EntityManager) = 
        //Console.Clear()
        //Console.SetBufferSize(MapWidth,MapHeight)

        //Console.SetBufferSize(Math.Max(MapWidth,Console.WindowWidth), Math.Max(MapHeight,Console.WindowHeight))

        Console.Title <- "World Map"
        
        let rangeY = 
            [|(snd _windowLocation)..(snd _windowLocation + viewSizeY - 1uy)|]

        let rangeX = 
            [|(fst _windowLocation)..(fst _windowLocation + viewSizeX - 1uy)|]

        for y in rangeY do
            for x in rangeX do
                let fs = 
                    enm.EntitiesAtLocation { X = x; Y = y; Z = 0uy } 
                    |> enm.TryGetComponentForEntities<FormComponent> 
                let f = fs.[fs.Length-1]
                System.Console.SetCursorPosition(int (x - fst _windowLocation), int (y - snd _windowLocation))
                System.Console.Write(f.Symbol)



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
        
