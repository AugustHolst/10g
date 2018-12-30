#load "animalsSmall.fs"

let runExperiment ticks fileName boardWidth NMooses mooseRepLen NWolves wolvesRepLen wolvesHungLen = 
    let isle = animals.environment(boardWidth, NMooses, mooseRepLen, NWolves, wolvesRepLen, wolvesHungLen, false)
    let resultFile = System.IO.File.CreateText fileName
    for i = 0 to ticks do
        resultFile.WriteLine (sprintf "moose population = %d | wolf population = %d" isle.board.moose.Length isle.board.wolves.Length)
        isle.tick()
    resultFile.Close()

let runExperimentToTikz ticks fileName boardWidth NMooses mooseRepLen NWolves wolvesRepLen wolvesHungLen =
    let isle = animals.environment(boardWidth, NMooses, mooseRepLen, NWolves, wolvesRepLen, wolvesHungLen, false)
    let mutable mooseData : (int * int) list = List.empty<int*int>
    let mutable wolvesData : (int * int) list = List.empty<int*int>
    for i = 0 to ticks do 
        mooseData <- (i, isle.board.moose.Length) :: mooseData
        wolvesData <- (i, isle.board.wolves.Length) :: wolvesData
        isle.tick()
    mooseData <- List.rev mooseData
    wolvesData <- List.rev wolvesData
    let resultFile = System.IO.File.CreateText fileName
    resultFile.WriteLine "\\begin{center}"
    resultFile.WriteLine "\\begin{tikzpicture}"
    resultFile.WriteLine "  \\begin{axis}[xlabel=Ticks, ylabel=Population, xmin=0, ymin=0]"
    
    resultFile.WriteLine "      \\addplot[color=red, mark=x] coordinates {"
    List.iter (fun tup -> resultFile.WriteLine (sprintf "       %A" tup)) mooseData
    resultFile.WriteLine "      };"
    
    resultFile.WriteLine "      \\addplot[color=blue, mark=o] coordinates {"
    List.iter (fun tup -> resultFile.WriteLine (sprintf "       %A" tup)) wolvesData
    resultFile.WriteLine "      };"
    
    resultFile.WriteLine "  \\legend{Moose, Wolves}"
    resultFile.WriteLine "  \\end{axis}"
    resultFile.WriteLine "\\end{tikzpicture}"
    resultFile.WriteLine "\\end{center}"
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
    
    ///run this instead for a graph made with LaTex tikzpicture and pgfplots
    //runExperimentToTikz ticks fileName boardWidth NMooses mooseRepLen NWolves wolvesRepLen wolvesHungLen 
    
    0 //0 needs to be returned for a succesful run. I think.
