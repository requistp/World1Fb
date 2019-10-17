module CalendarTimings


let secondsPerRound = 6
let roundsPerMinute = 60 / secondsPerRound // 10 rounds
let minutesPerHour = 60  //       600 rounds
let hoursPerDay = 24     //    14,400 rounds
let daysPerMonth = 30    //   432,000 rounds
let monthsPerYear = 12   // 5,184,000 rounds

let roundsPerHour = roundsPerMinute * minutesPerHour
let roundsPerDay = roundsPerHour * hoursPerDay
let roundsPerMonth = roundsPerDay * daysPerMonth
let roundsPerYear = roundsPerMonth * monthsPerYear


