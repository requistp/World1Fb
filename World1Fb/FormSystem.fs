module FormSystem
open Components
open GameManager
open EntityComponentManager

type FormSystem(isActive:bool, newEntities:ComponentType list list) =
    inherit AbstractSystem(isActive,false) 
   
    override this.Initialize = 
        match this.IsActive with
        | false -> List.empty
        | true -> base.SetToInitialized
                  newEntities |> List.collect (fun ctl -> [EntityAddition ctl]) 

    override this.Update ecm = 
        List.empty
