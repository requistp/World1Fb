module FormSystem
open System_Abstract
open EntityComponentManager

type FormSystem(isActive:bool) =
    inherit AbstractSystem(isActive,false) 
   
    override this.Initialize = EntityComponentManager()

    override this.Update ecm = 
        ecm
