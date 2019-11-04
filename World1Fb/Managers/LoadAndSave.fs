module LoadAndSave
open MBrace.FsPickler


// Round -> EventManager.GetRound()
// Entity Component Map -> EntityManager.GetMap
// Max EntityID -> EntityManager.GetMaxID
// Scheduled Events -> EventManager.GetSchedule
// Registered Systems -> SystemManager.GetSystems



let binarySerializer = FsPickler.CreateBinarySerializer()
let xmlSerializer = FsPickler.CreateXmlSerializer(indent = true)


