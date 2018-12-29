#load "animalsSmall.fs"

let isle = animals.environment(10, 6, 5, 3, 5, 6, false)
let ticks = 40
for t=0 to ticks do
    printfn "%A" isle // The inital board
    isle.tick() // This is a mockup method for now...
    printfn "\ntick number: %d" (t+1)