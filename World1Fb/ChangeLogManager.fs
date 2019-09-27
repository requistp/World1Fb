module ChangeLogManager
open CommonGenericFunctions
open Components

[<Struct>]
type FrameChangeLog(ccl:Map<uint32,ComponentChangeType list>, entityAdditions:Map<uint32,ComponentType list>, entityRemovals:uint32 list) =
    static member New = FrameChangeLog(Map.empty,Map.empty,List.empty)
    member this.ComponentChangeLog = ccl
    member this.EntityAdditions = entityAdditions
    member this.EntityRemovals = entityRemovals

    member this.AddComponentChange (e:uint32) (cct:ComponentChangeType) =
        match ccl.ContainsKey(e) with
        | true -> FrameChangeLog(ccl.Add(e,cct::ccl.Item(e)), entityAdditions, entityRemovals)
        | false -> FrameChangeLog(ccl.Add(e,[cct]), entityAdditions, entityRemovals)
    member this.AddEntity (ctl:ComponentType list) = 
        match ctl.IsEmpty with
        | true -> Failure "ctl is empty"
        | false -> Success FrameChangeLog(ccl, entityAdditions.Add(uint32 (entityAdditions.Count+1),ctl), entityRemovals)
    member this.RemoveEntity (e:uint32) = 
        FrameChangeLog(ccl, entityAdditions, e::entityRemovals)

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

    member this.AddEntity (ctl:ComponentType list) = 
        match _fcl.AddEntity with
        | Failure x -> Failure x
        | Success fcl -> _fcl <- fcl
                         Success "added"

    member this.AddEntities_ViaTuple (i,ctl:ComponentType list) list =
        1

    member this.AddEntities_ViaComponentTypeList (ctl:ComponentType list) =
        for ct in ctl do
            i <- i + 1u
            let ec = EntityComponent(i,ct)
            newmap <- ecmap.Add(i,[ec])
            newcd <- maintainComponentDict_Add newcd ec
        EntityComponentManager(newmap, i, newcd)