﻿module LoadAndSave
open agent_Components
open agent_ComponentTypes
open agent_EntityManager
open agent_Locations
open CommonGenericFunctions
open Component
open ComponentEnums
open EventTypes
open LocationTypes
open MBrace.FsPickler
open System
open System.IO

type SaveGameData = 
    { 
        Components : Save_Components
        ComponentTypes : Save_ComponentTypes
        Entities : Save_Entities
        Locations : Save_Locations
        Round : RoundNumber
        ScheduledEvents : Map<RoundNumber,ScheduledEventData[]>
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


let private outputStream (format:SaveGameFormats) (round:RoundNumber) = 
    if Directory.Exists(savePath) |> not then Directory.CreateDirectory(savePath) |> ignore
    let str = 
        let filename = savePath + "/" + "Save_" +  (DateTime.Now.ToString("yyyyMMddHHmm")) + "_r" + round.ToUint32.ToString() + format.Ext
        match format with
        | Binary -> new StreamWriter(File.OpenWrite(filename))
        | XML -> new StreamWriter(File.OpenWrite(filename))
    str.AutoFlush <- true
    str.BaseStream
    

let LoadGame (format:SaveGameFormats) (filename:string) =
    match format with
    | Binary -> binarySerializer.Deserialize<SaveGameData> (inputStream format filename)
    | XML -> xmlSerializer.Deserialize<SaveGameData> (inputStream format filename)


let SaveGame (format:SaveGameFormats) (sgd:SaveGameData) (round:RoundNumber) = 
    match format with
    | Binary -> binarySerializer.Serialize(outputStream format round, sgd)
    | XML -> xmlSerializer.Serialize(outputStream format round, sgd)


