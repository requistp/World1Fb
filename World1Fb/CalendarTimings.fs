module CalendarTimings
open CommonGenericFunctions
open System

type FrequencyTypes = 
    | Minute
    | Hour
    | Day
    | Month
    | Year

let secondsPerMinute = 60
let minutesPerHour = 60  
let hoursPerDay = 24     
let daysPerMonth = 30    
let monthsPerYear = 12   

let roundInSeconds = 6
let roundsPerMinute = secondsPerMinute / roundInSeconds //        10 rounds
let roundsPerHour = roundsPerMinute * minutesPerHour    //       600 rounds
let roundsPerDay = roundsPerHour * hoursPerDay          //    14,400 rounds
let roundsPerMonth = roundsPerDay * daysPerMonth        //   432,000 rounds
let roundsPerYear = roundsPerMonth * monthsPerYear      // 5,184,000 rounds


let convertRounds (r:int) (toFreq:FrequencyTypes) = 
    match toFreq with
    | Minute -> (float r) / (float roundsPerMinute)
    | Hour -> (float r) / (float roundsPerHour)
    | Day -> (float r) / (float roundsPerDay)
    | Month -> (float r) / (float roundsPerMonth)
    | Year -> (float r) / (float roundsPerYear)


let convertAmountByFrequency (amountPerFreq:int) (frequnecyOfAmount:FrequencyTypes) (perIntervals:int) = 
    int (Math.Round((float amountPerFreq) * convertRounds perIntervals frequnecyOfAmount, 0))
   

let ExecuteTiming (frequency:int) (offset:int) (round:int) = 
    printfn "F:%i O:%i R:%i" frequency offset round
    match frequency with
    | 0 -> false
    | _ -> (round % frequency = offset)

   
let TimingOffset (max:int) = random.Next(0, max-1)

