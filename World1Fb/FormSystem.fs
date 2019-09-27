module FormSystem
open Components
open System_Abstract
open EntityComponentManager

type FormSystem(isActive:bool) =
    inherit AbstractSystem(isActive,false) 
   
    override this.Initialize = 
        FrameChangeLog(Map.empty, Map.empty, List.empty)

    override this.Update ecm = 
        FrameChangeLog(Map.empty, Map.empty, List.empty)
