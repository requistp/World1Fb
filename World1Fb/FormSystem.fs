module FormSystem
open Components
open GameManager
open EntityComponentManager

type FormSystem(isActive:bool) =
    inherit AbstractSystem(isActive) 
   
    override this.Initialize = 
        base.SetToInitialized
        ()
        //match this.IsActive with
        //| false -> List.empty
        //| true -> base.SetToInitialized
        //          newEntities |> List.collect (fun ctl -> [EntityAddition ctl]) 

    override this.Update ecm = 
        () //List.empty
