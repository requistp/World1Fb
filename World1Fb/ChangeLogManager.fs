module ChangeLogManager
//open CommonGenericFunctions
//open Components

//[<Struct>]
//type ChangeLog(ccl:Map<uint32,ComponentChangeType list>, entityAdditions:Map<uint32,ComponentType list>, entityRemovals:uint32 list) =
//    static member New = ChangeLog(Map.empty,Map.empty,List.empty)
//    member this.ComponentChangeLog = ccl
//    member this.EntityAdditions = entityAdditions
//    member this.EntityRemovals = entityRemovals

//    //member this.AddComponentChange (e:uint32) (cct:ComponentChangeType) =
//    //    match ccl.ContainsKey(e) with
//    //    | true -> ChangeLog(ccl.Add(e,cct::ccl.Item(e)), entityAdditions, entityRemovals)
//    //    | false -> ChangeLog(ccl.Add(e,[cct]), entityAdditions, entityRemovals)
//    //member this.AddEntity (ctl:ComponentType list) = 
//    //    match ctl.IsEmpty with
//    //    | true -> this
//    //    | false -> ChangeLog(ccl, entityAdditions.Add(uint32 (entityAdditions.Count+1),ctl), entityRemovals)
//    //member this.RemoveEntity (e:uint32) = 
//    //    match entityRemovals |> List.exists (fun i -> i = e) with
//    //    | true -> this
//    //    | false -> ChangeLog(ccl, entityAdditions, e::entityRemovals)

//let ChangeLog_AddComponentChange (cl:ChangeLog) (e:uint32) (cct:ComponentChangeType) =
//    match cl.ComponentChangeLog.ContainsKey(e) with
//    | true -> ChangeLog(cl.ComponentChangeLog.Add(e,cct::cl.ComponentChangeLog.Item(e)), cl.EntityAdditions, cl.EntityRemovals)
//    | false -> ChangeLog(cl.ComponentChangeLog.Add(e,[cct]), cl.EntityAdditions, cl.EntityRemovals)

//let ChangeLog_AddEntity (cl:ChangeLog) (ctl:ComponentType list) = 
//    match ctl.IsEmpty with
//    | true -> cl
//    | false -> ChangeLog(cl.ComponentChangeLog, cl.EntityAdditions.Add(uint32 (cl.EntityAdditions.Count+1),ctl), cl.EntityRemovals)
////let ChangeLog_AddEntity_SingleComponent (cl:ChangeLog) (ct:ComponentType) = this.AddEntity [ct]
////    member this.RemoveEntity (e:uint32) =
////        _cl <- (_cl.RemoveEntity e)
////        Ok "done"
//let ChangeLog_AddEntities_ViaComponentTypeList (ctl:ComponentType list) =
//        let _ = ctl |> List.map this.AddEntity_SingleComponent
////        //match r |> List.exists (fun e -> IsFailure e) with
////        //| true -> let fl = r |> List.filter (fun e -> IsFailure e) |> List.collect FailurePart
////        //          //let fl2 = fl.
////        //          Failure 1
////        //| false -> Success "done"
////        Ok "done"

//let ChangeLog_RemoveEntity (cl:ChangeLog) (e:uint32) = 
//    match cl.EntityRemovals |> List.exists (fun i -> i = e) with
//    | true -> cl
//    | false -> ChangeLog(cl.ComponentChangeLog, cl.EntityAdditions, e::cl.EntityRemovals)

//type ChangeLogManager() = 
//    let mutable _cl = ChangeLog.New

//    member this.AddComponentChange  (e:uint32) (cct:ComponentChangeType) =
//        let newfcl = _cl.AddComponentChange e cct
//        _cl <- newfcl
//        Ok "done"
//    member this.AddEntity (ctl:ComponentType list) = 
//        _cl <- (_cl.AddEntity ctl)
//        Ok "done"
//    member this.AddEntity_SingleComponent (ct:ComponentType) = this.AddEntity [ct]
//    member this.RemoveEntity (e:uint32) =
//        _cl <- (_cl.RemoveEntity e)
//        Ok "done"

//    member this.AddEntities_ViaComponentTypeList (ctl:ComponentType list) =
//        let _ = ctl |> List.map this.AddEntity_SingleComponent
//        //match r |> List.exists (fun e -> IsFailure e) with
//        //| true -> let fl = r |> List.filter (fun e -> IsFailure e) |> List.collect FailurePart
//        //          //let fl2 = fl.
//        //          Failure 1
//        //| false -> Success "done"
//        Ok "done"
