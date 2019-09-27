module ChangeLogManager
open Components

[<Struct>]
type FrameChangeLog(ccl:Map<uint32,ComponentChangeType list>, entityAdditions:Map<string,ComponentType list>, entityRemovals:uint32 list) =
    static member New = FrameChangeLog(Map.empty,Map.empty,List.empty)
    member this.ComponentChangeLog = ccl
    member this.EntityAdditions = entityAdditions
    member this.EntityRemovals = entityRemovals

type ChangeLogManager() = 
    let mutable _fcl = FrameChangeLog.New

    //let CTranslateMapToFrameChangeLog (ctl:ComponentType list) = 
    //    let mm2 = List.init MakeMap.Length (fun index -> index)
    //    printfn "%O" mm2.ToString
    //    let t = List.map2 (fun k v -> (k,v)) mm2 MakeMap
    //    //let mm2 = MakeMap |> List.map (fun x -> let mutable i = 0; (i,x))
    //    //let x = MakeMap |> Map.ofList MakeMap.It
    //    //entityAdditions:Map<string,ComponentType list>
    //    FrameChangeLog(Map.empty, Map.empty, List.empty

    member this.CreateEntities_ViaComponentTypeList (ctl:ComponentType list) =

        ctl |> List.indexed

        let mutable i = maxEntityID
        let mutable newmap = ecmap
        let mutable newcd = compDict

        for ct in ctl do
            i <- i + 1u
            let ec = EntityComponent(i,ct)
            newmap <- ecmap.Add(i,[ec])
            newcd <- maintainComponentDict_Add newcd ec
        EntityComponentManager(newmap, i, newcd)