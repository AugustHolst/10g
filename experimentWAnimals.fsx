#load "animalsSmall.fs"

let runExperiment ticks fileName boardWidth NMooses mooseRepLen NWolves wolvesRepLen wolvesHungLen = 
    let isle = animals.environment(boardWidth, NMooses, mooseRepLen, NWolves, wolvesRepLen, wolvesHungLen, false)
    let resultFile = System.IO.File.CreateText fileName
    for i = 1 to ticks do
        isle.tick()
        resultFile.WriteLine (sprintf "moose population = %d | wolf population = %d" isle.board.moose.Length isle.board.wolves.Length)
    resultFile.Close()
///This program takes 8 different arguments as described in 10g.pdf (d)
///Example use: mono experimentWAnimals.exe 40 test.txt 10 30 10 2 10 4
[<EntryPoint>]
let main args = 
    printfn "args = %A" args
    let ticks = int args.[0]
    let fileName = args.[1]
    let boardWidth = int args.[2]
    let NMooses = int args.[3]
    let mooseRepLen = int args.[4]
    let NWolves = int args.[5]
    let wolvesRepLen = int args.[6]
    let wolvesHungLen = int args.[7]
    runExperiment ticks fileName boardWidth NMooses mooseRepLen NWolves wolvesRepLen wolvesHungLen
    0
