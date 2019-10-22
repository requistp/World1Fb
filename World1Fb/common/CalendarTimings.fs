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

let convertAmountByFrequency (amountPerFreq:int) (frequencyOfAmount:FrequencyTypes) (perIntervals:int) = 
    int (Math.Round((float amountPerFreq) * convertRounds perIntervals frequencyOfAmount, 0))
   
let ExecuteTiming (frequency:int) (offset:int) (round:int) = 
    match frequency with
    | 0 -> false
    | _ -> (round % frequency = offset)

let TimingOffset (max:int) = 1 + random.Next(0,max) // Add 1 because the frequency timing is being done off the current round, but that round is really over. So a zero offset should be done next round

let AllTimingAccelerator = 100
let MetabolismFrequency = roundsPerHour * 2 / AllTimingAccelerator
let PlantGrowthFrequency = roundsPerDay / AllTimingAccelerator // If I change this, I need to change the regrowRate because 100% of that is applied per this update
let PlantReproductionFrequency = roundsPerMonth / AllTimingAccelerator // If I change this, I need to change the reproductionRate because 100% of that is applied per this update



