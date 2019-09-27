module ChangeLogManager
open Components

[<Struct>]
type FrameChangeLog(ccl:Map<uint32,ComponentChangeType list>, entityAdditions:Map<string,ComponentType list>, entityRemovals:uint32 list) =
    static member New = FrameChangeLog(Map.empty,Map.empty,List.empty)
    member this.ComponentChangeLog = ccl
    member this.EntityAdditions = entityAdditions
    member this.EntityRemovals = entityRemovals

