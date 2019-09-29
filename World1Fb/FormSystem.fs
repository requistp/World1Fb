module FormSystem
open Components
open GameManager
open EntityComponentManager

type FormSystem(isActive:bool) =
    inherit AbstractSystem(isActive,false) 
   
    override this.Initialize = 
        base.SetToInitialized
        match this.IsActive with
        | true -> List.empty
        | false -> List.empty

    override this.Update ecm = 
        List.empty
