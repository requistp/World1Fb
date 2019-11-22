module LoadAndSave
open CommonGenericFunctions
open Component
open EntityManager
open EventTypes
open LocationTypes
open MBrace.FsPickler
open System
open System.IO

type SaveGameData = 
    { 
        Entities : 
            (Map<ComponentID,Component> * Map<ComponentID,(RoundNumber*Component option)[]>)
            *
            (Map<ComponentTypeID,ComponentID[]> * Map<ComponentTypeID,(RoundNumber*ComponentID[] option)[]>)
            *
            (Map<EntityID,ComponentID[]> * Map<EntityID,(RoundNumber*ComponentID[] option)[]>)
            *
            (Map<LocationDataInt,ComponentID[]> * Map<LocationDataInt,(RoundNumber*ComponentID[] option)[]>)
        Round : RoundNumber
        ScheduledEvents : Map<RoundNumber,GameEventTypes[]>
    }
    
type SaveGameFormats =
    | Binary
    | XML
    member me.Ext =
        match me with   
        | Binary -> ".bin"
        | XML -> ".xml"


let private savePath = "./saves"
let private binarySerializer = FsPickler.CreateBinarySerializer()
let private xmlSerializer = FsPickler.CreateXmlSerializer(indent = true)


let private inputStream format filename = 
    let str = 
        match format with
        | Binary -> new StreamReader(File.OpenRead(savePath + "/" + filename + format.Ext))
        | XML -> new StreamReader(File.OpenRead(savePath + "/" + filename + format.Ext))
    str.BaseStream


let private outputStream format = 
    if Directory.Exists(savePath) |> not then Directory.CreateDirectory(savePath) |> ignore
    let str = 
        match format with
        | Binary -> new StreamWriter(File.OpenWrite(savePath + "/" + "Save_" + (DateTime.Now.ToString("yyyyMMddHHmm")) + format.Ext))
        | XML -> new StreamWriter(File.OpenWrite(savePath + "/" + "Save_" +  (DateTime.Now.ToString("yyyyMMddHHmm")) + format.Ext))
    str.AutoFlush <- true
    str.BaseStream
    

let LoadGame (format:SaveGameFormats) (filename:string) =
    match format with
    | Binary -> binarySerializer.Deserialize<SaveGameData> (inputStream format filename)
    | XML -> xmlSerializer.Deserialize<SaveGameData> (inputStream format filename)


let SaveGame (format:SaveGameFormats) (sgd:SaveGameData) = 
    match format with
    | Binary -> binarySerializer.Serialize(outputStream format, sgd)
    | XML -> xmlSerializer.Serialize(outputStream format, sgd)


