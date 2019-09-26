module FormSystem
open SystemManager

type FormSystem(isActive:bool) =
    inherit AbstractSystem(isActive,false) 
   
    override this.Initialize ecm = ecm

    override this.Update dt ecm = 
        ecm
