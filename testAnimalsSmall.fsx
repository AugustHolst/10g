#load "animalsSmall.fs"

let isle = animals.environment(10, 6, 5, 3, 5, 6, false)
let ticks = 40
for t=1 to ticks do
    printfn "%A" isle
    isle.tick()
    printfn "\ntick number: %d" t