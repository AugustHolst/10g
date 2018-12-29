module animals
open System

type symbol = char
type position = int * int
type neighbour = position * symbol option

let mSymbol : symbol = 'm'
let wSymbol : symbol = 'w'
let eSymbol : symbol = ' '
let rnd = System.Random ()

/// An animal is a base class. It has a position and a reproduction counter.
[<AbstractClass>]
type animal (symb : symbol, repLen : int) =
  let mutable _reproduction = rnd.Next(1,repLen)
  let mutable _pos : position option = None
  let mutable _neighbours : neighbour list = []
  let _symbol : symbol = symb

  member this.symbol = _symbol
  member this.position
    with get () = _pos
    and set aPos = _pos <- aPos
  member this.reproduction = _reproduction
  member this.updateReproduction () =
    _reproduction <- _reproduction - 1
  member this.resetReproduction () =
    _reproduction <- repLen
  
  //Own neighbours implementation.
  member this.neighbours
    with get () = _neighbours
    and set nb = _neighbours <- nb

  //Own move method implementation.
  member this.move () = 
    let availableTiles = List.filter (fun nb -> (snd nb) = Some ' ') this.neighbours
    if availableTiles.Length <> 0 then
      let newPos = Some (fst availableTiles.[rnd.Next(0, availableTiles.Length)])
      this.position <- newPos

  override this.ToString () =
    string this.symbol

/// A moose is an animal
type moose (repLen : int) =
  inherit animal (mSymbol, repLen)
  
  member this.reproduce () = 
    let availableTiles = List.filter (fun nb -> (snd nb) = Some ' ') this.neighbours
    let newPos = Some (fst availableTiles.[rnd.Next(0, availableTiles.Length)])
    let newborn : moose = new moose (repLen)
    newborn.position <- newPos
    this.resetReproduction()
    newborn

  member this.tick () : moose option =
    let availableTiles = List.filter (fun nb -> (snd nb) = Some ' ') this.neighbours
    match this.reproduction, availableTiles.Length with
    |0, 0 -> None                                                                     //moose has no place to give birth.
    |0, _ -> Some (this.reproduce())                                                  //moose gives birth
    |_, 0 -> this.updateReproduction(); None                                          //moose has no place to move.
    |_, _ -> this.updateReproduction(); base.move(); None;                            //moose moves.

/// A wolf is an animal with a hunger counter
type wolf (repLen : int, hungLen : int) =
  inherit animal (wSymbol, repLen)
  
  let mutable _hunger = hungLen
  member this.hunger = _hunger
  member this.updateHunger () =
    _hunger <- _hunger - 1
    if _hunger <= 0 then
      this.position <- None // Starve to death
  member this.resetHunger () =
    _hunger <- hungLen
  
  member this.reproduce () =
    let availableTiles = List.filter (fun nb -> (snd nb) = Some ' ') this.neighbours
    let newPos = Some (fst availableTiles.[rnd.Next(0, availableTiles.Length)])
    let newborn : wolf = new wolf (repLen, hungLen)
    newborn.position <- newPos
    this.resetReproduction()
    newborn

  member this.kill () = 
    let neighbouringMoose = List.filter (fun nb -> (snd nb) = Some 'm') this.neighbours
    if neighbouringMoose.IsEmpty then None
    else
      let moosePos = Some (fst neighbouringMoose.[rnd.Next(0, neighbouringMoose.Length)])
      this.position <- moosePos
      this.resetHunger()
      moosePos

  member this.tick () : wolf option =
    let availableTiles = List.filter (fun nb -> (snd nb) = Some ' ') this.neighbours
    this.updateHunger()
    if this.position.IsSome then  //this check is done because the wolf can die of hunger before moving or reproducing.
      match this.reproduction, availableTiles.Length with
      |0, 0 -> None                                                                     //wolf has no place to give birth.
      |0, _ -> Some (this.reproduce())                                                  //wolf gives birth
      |_, 0 -> this.updateReproduction(); None                                          //wolf has no place to move.
      |_, _ -> this.updateReproduction(); base.move(); None;                            //wolf moves.
    else None

/// A board is a chess-like board implicitly representedy by its width and coordinates of the animals.
type board =
  {width : int;
   mutable moose : moose list;
   mutable wolves : wolf list;
   }

/// An environment is a chess-like board with all animals and implenting all rules.
type environment (boardWidth : int, NMooses : int, mooseRepLen : int, NWolves : int, wolvesRepLen : int, wolvesHungLen : int, verbose : bool) =
  let _board : board = {
    width = boardWidth;
    moose = List.init NMooses (fun i -> moose(mooseRepLen));
    wolves = List.init NWolves (fun i -> wolf(wolvesRepLen, wolvesHungLen));
  }
  
  /// Holds both moose and wolves.
  let mutable animals : array<animal> = List.toArray (List.map(fun m -> upcast m) _board.moose @ (List.map(fun w -> upcast w) _board.wolves))

  /// Fisher-Yates shuffle https://en.wikipedia.org/wiki/Fisher%E2%80%93Yates_shuffle
  /// Used to randomize the order in which every animal's action is taken.
  let FYShuffle (arr : 'a array) =
    for i = arr.Length-1 downto 1 do
      let j = rnd.Next(0, i)
      let temp = arr.[j] 
      arr.[j] <- arr.[i]
      arr.[i] <- temp

  /// Project the list representation of the board into a 2d array.
  let draw (b : board) : char [,] =
    let arr = Array2D.create<char> boardWidth boardWidth eSymbol
    for m in b.moose do
      Option.iter (fun p -> arr.[fst p, snd p] <- mSymbol) m.position
    for w in b.wolves do
      Option.iter (fun p -> arr.[fst p, snd p] <- wSymbol) w.position
    arr

  /// return the coordinates of any empty field on the board.
  let anyEmptyField (b : board) : position =
    let arr = draw b
    let mutable i = rnd.Next b.width
    let mutable j = rnd.Next b.width
    while arr.[i,j] <> eSymbol do
      i <- rnd.Next b.width
      j <- rnd.Next b.width
    (i,j)
 
  // helping function used in assigning neighbours
  let positionToSymbol (pos : position) (symbols : char [,]) : symbol option = 
    try
      Some symbols.[fst pos, snd pos]
    with
      | :? System.IndexOutOfRangeException -> None

  // assigns neighbours for an animal
  let assignNeighbours (a : animal) = 
    let arr = draw _board
    let posX = fst a.position.Value
    let posY = snd a.position.Value
    a.neighbours <- [
      ((posX-1,posY-1), (positionToSymbol (posX-1, posY-1) arr))
      ((posX-1,posY), (positionToSymbol (posX-1, posY) arr))
      ((posX-1,posY+1), (positionToSymbol (posX-1, posY+1) arr))

      ((posX,posY-1), (positionToSymbol (posX, posY-1) arr))
      ((posX,posY+1), (positionToSymbol (posX, posY+1) arr))

      ((posX+1,posY-1), (positionToSymbol (posX+1, posY-1) arr))
      ((posX+1,posY), (positionToSymbol (posX+1, posY) arr))
      ((posX+1,posY+1), (positionToSymbol (posX+1, posY+1) arr))
    ]
    
  // populate the board with animals placed at random.
  do for m in _board.moose do
       m.position <- Some (anyEmptyField _board)
  do for w in _board.wolves do
       w.position <- Some (anyEmptyField _board)

  member this.size = boardWidth*boardWidth
  member this.count = _board.moose.Length + _board.wolves.Length
  member this.board = _board
  member this.tick () = 
    let helpTick (a : animal) = 
      if a.position.IsSome then
        assignNeighbours a
        match a.symbol with 
        | 'm' -> 
          let m = a :?> moose                                                               //the :?> operator dynamically downcasts to given type.
          let offspring = m.tick()
          if offspring.IsSome then _board.moose <- offspring.Value :: _board.moose; animals <- Array.append animals [|(downcast offspring.Value)|]
        | 'w' -> 
          let w = a :?> wolf
          let neighbouringMoose = List.filter (fun nb -> (snd nb) = Some 'm') w.neighbours
          if neighbouringMoose.IsEmpty then
            let offspring = w.tick()
            if offspring.IsSome then _board.wolves <- offspring.Value :: _board.wolves; animals <- Array.append animals [|(downcast offspring.Value)|] 
          else 
            let targetPos = w.kill()
            let isMooseInTarPos (m : moose) = 
              try
                m.position = targetPos
              with 
                | :? System.NullReferenceException -> false
            let deadMoose = List.find (isMooseInTarPos) _board.moose
            deadMoose.position <- None
    FYShuffle animals
    Array.iter (helpTick) animals
    _board.moose <- List.filter (fun m -> m.position.IsSome) _board.moose
    _board.wolves <- List.filter (fun w -> w.position.IsSome) _board.wolves
    animals <- Array.filter (fun a -> a.position.IsSome) animals

  override this.ToString () =
    let arr = draw _board
    let mutable ret = "  "
    for j = 0 to _board.width-1 do
      ret <- ret + string (j % 10) + " "
    ret <- ret + "\n"
    for i = 0 to _board.width-1 do
      ret <- ret + string (i % 10) + " "
      for j = 0 to _board.width-1 do
        ret <- ret + string arr.[i,j] + " "
      ret <- ret + "\n"
    ret
