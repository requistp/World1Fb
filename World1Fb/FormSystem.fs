module FormSystem
open ChangeLogManager
open Components
open System_Abstract
open EntityComponentManager

type FormSystem(isActive:bool) =
    inherit AbstractSystem(isActive,false) 
   
    override this.Initialize  (clm:ChangeLogManager) = 
        Ok "done"

    override this.Update ecm = 
        ChangeLog(Map.empty, Map.empty, List.empty)
