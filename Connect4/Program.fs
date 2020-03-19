// Learn more about F# at http://fsharp.org

open System

let NumRows = 6
let NumCols = 7
let NumToWin = 4

let makeBoard =
     Array2D.init<char> NumRows NumCols (fun row col -> ' ')

//https://en.wikibooks.org/wiki/F_Sharp_Programming/Arrays
let printBoard board isOver =
    
    let maxY = (Array2D.length1 board) - 1
    let maxX = (Array2D.length2 board) - 1
    
    match isOver with
    | true -> for row in 0 .. maxY do
                for col in 0 .. maxX do    
                    printf "| "
                    printf "%c" ' '
                    printf " "
                Console.WriteLine()
    | false -> for row in 0 .. maxY do
                 for col in 0 .. maxX do
                    printf "| "
                    printf "%c" board.[row, col]
                    printf " "
                 Console.WriteLine()
    
    match isOver with
    | false -> printfn "---------------------------"
    | true -> printfn "  O      X    O    X     O   "
              printfn "      O    O     X      O   "
              printfn "           X      X        O   "
              printfn "  X             O      X         X"
              printfn "      O            O     X    O "
              printfn "                X           X     O "
              printfn "            X    X           X     O "
              printfn "                      O               O "
              printfn "                  O            X           X"
              printfn "                       O                  "

let addCharToCol board player colIndex =
    
    let rec _addCharToCol board player colIndex currRow =
        match currRow with
        | currRow when currRow >= (Array2D.length1 board) -> true
        | currRow when (board.[currRow, colIndex] = 'X') || (board.[currRow, colIndex] = 'O') -> true
        | currRow when (_addCharToCol board player colIndex (currRow + 1)) -> (board.[currRow, colIndex] <- player)
                                                                              false
        | _ -> false
        
    _addCharToCol board player colIndex 0 |> ignore
    ()

let rec playerMakeMove board player=
    printf "Player "
    printf "%c" player
    printfn " make a move."
    
    let targetCol = Console.ReadLine() |> int
    
    if targetCol > Array2D.length1 board then printfn "Illegal move. try again"
                                              Console.WriteLine()
                                              printBoard board false
                                              playerMakeMove board player
                                              
    if board.[0, targetCol] = 'O' || board.[0, targetCol] = 'X' then printfn "column full. try again."
                                                                     Console.WriteLine()
                                                                     printBoard board false
                                                                     playerMakeMove board player
    
    addCharToCol board player targetCol
                                                                      
let opposite player =
    match player with
    | 'X' -> 'O'
    | 'O' -> 'X'
    | _ -> 'Q'
      
let rec getCol (board: char[,]) rowIndexList (colIndex: int) answer=
//    board.[*, colIndex]
    match rowIndexList with
    | [] -> answer
    | hd::tl -> getCol board tl colIndex (board.[hd, colIndex] :: answer) 

let getAllCols board =
    let matrixWidth = (Array2D.length2 board) //the length of a row (left and right)
    let matrixHeight = (Array2D.length1 board) //the length of a col (height)
    
    let rowIndexList = [0..(matrixHeight - 1)] //index of each row (up and down)
    let colIndexList = [0..(matrixWidth - 1)] //index of each col (left and right)
    
    //go over each col
    let rec _getAllCols board colIndexList listOfCols =
        match colIndexList with
        | [] -> listOfCols
        | hd::tl -> _getAllCols board tl (listOfCols @ [( getCol board rowIndexList hd [] )]  )
  
    _getAllCols board colIndexList []

let rec getRow (board: char [,]) colIndexList (rowIndex: int) answer =
    match colIndexList with
    | [] -> answer
    | hd::tl -> getRow board tl rowIndex (board.[rowIndex, hd] :: answer)

let getAllRows board =
    let matrixWidth = (Array2D.length2 board) //the length of a row (left and right)
    let matrixHeight = (Array2D.length1 board) //the length of a col (height)
    
    let rowIndexList = [0..(matrixHeight - 1)] //index of each row (up and down)
    let colIndexList = [0..(matrixWidth - 1)] //index of each col (left and right)
    
    //go over each rows
    let rec _getAllRows board rowIndexList listOfRows =
        match rowIndexList with
        | [] -> listOfRows
        | hd::tl -> _getAllRows board tl (listOfRows @ [( getRow board colIndexList hd [] )]  )
  
    _getAllRows board rowIndexList []

let allCombinations max =
    
    let allValues2 = [0..(max - 1)]
    
    let rec _getTuplesGivenHd allValues givenHd acc =
        match allValues with
        | [] -> acc
        | hd::tl -> 
                    _getTuplesGivenHd tl givenHd ((givenHd, hd) :: acc) 
        
    
    let rec _allCombinations allValues acc =
        match allValues with
        | [] -> acc
        | hd::tl -> _allCombinations tl (_getTuplesGivenHd allValues2 hd [] @ acc)

    _allCombinations allValues2 []
    

let getAllDiagonalForward (board: char [,]) =
//    printfn "hey1"
    let dimensionList = [NumCols; NumRows]
    let allCombos = allCombinations (List.max dimensionList) 
    let acceptableCombos = List.filter (fun (x, y) -> x < NumCols && y < NumRows) allCombos
    
    
    let allDiagonalSums = [0..(NumRows + NumCols - 2)]
    
    let rec _getDiagonals allDiagonalSums list =
//        printfn "Het"
        match allDiagonalSums with
        | [] -> list
        | hd::tl -> _getDiagonals tl (let diag = List.filter (fun (x, y) -> (x + y) = hd) acceptableCombos
//                                      printfn "%A" diag
                                      List.map (fun (x, y) -> board.[y, x]) diag :: list
                                     )
    _getDiagonals allDiagonalSums []                                                 

let xMap x =
    abs(x - (NumCols - 1))
    

let getAllDiagonalBackward (board: char [,]) =
//    printfn "hey1"
    let dimensionList = [NumCols; NumRows]
    let allCombos = allCombinations (List.max dimensionList) 
    let acceptableCombos = List.filter (fun (x, y) -> x < NumCols && y < NumRows) allCombos
    
    
    let allDiagonalSums = [0..(NumRows + NumCols - 2)]
    
    let rec _getDiagonals allDiagonalSums list =
//        printfn "Het"
        match allDiagonalSums with
        | [] -> list
        | hd::tl -> _getDiagonals tl (let diag = List.filter (fun (x, y) -> (x + y) = hd) acceptableCombos
//                                      printfn "%A" diag
                                      List.map (fun (x, y) -> board.[y, xMap (x)]) diag :: list
                                     )
    _getDiagonals allDiagonalSums [] 

let printWinMsg player =
    printf "Player "
    printf "%c" player
    printfn " Won. Yay."

let checkForWinnerInList L =
    
    let rec _checkForWinnerInList L streak player =
        match L  with
        | [] -> false
        | hd::tl when hd <> 'X' && hd <> 'O' -> _checkForWinnerInList tl 0 player
        | hd::_ when hd = player && (streak + 1) = NumToWin -> printWinMsg player
                                                               true
        | hd::tl when hd = player -> _checkForWinnerInList tl (streak + 1) (player)
        | hd::tl when hd <> player -> _checkForWinnerInList tl 1 (hd)
        | _ ->  printfn "Case: some weird char appears?"
                false
        
    _checkForWinnerInList L 0 'X'
    
let isWinner board =
    let allPossibleWinPaths = (getAllCols board) @ (getAllRows board) @ (getAllDiagonalForward board) @ (getAllDiagonalBackward board)
//    printfn "%A" (getAllRows board)
    let rec _checkAllWinPaths allPossibleWinPaths =
        match allPossibleWinPaths with
        | [] -> false
        | hd::tl when (checkForWinnerInList hd) -> true
        | _::tl -> _checkAllWinPaths tl
    
    _checkAllWinPaths allPossibleWinPaths
    
//let isTie board =
//    let matrixWidth = (Array2D.length2 board) //the length of a row (left and right)
//    let matrixHeight = (Array2D.length1 board) //the length of a col (height)
//    
//    let rowIndexList = [0..(matrixHeight - 1)] //index of each row (up and down)
//    let colIndexList = [0..(matrixWidth - 1)] //index of each col (left and right)
//
//    let topRow = (getRow board colIndexList 0 [])
//    printfn "%A" topRow
//    if (List.tryFind (fun c -> c = ' ') topRow) <> None then false else  printfn("There was a tie. No one wins obviously.")
//                                                                         true
//     
[<EntryPoint>]
let main argv =
    printfn "Connect 4. Prepare to have fun."
    
    let board = makeBoard
    
    printBoard board false
//    printBoard board true
    
    let rec _playGame board playerTurn =
        match playerTurn with
        | 'X' -> playerMakeMove board 'X'
        | 'O' -> playerMakeMove board 'O'
        | _ -> ()
        
        printBoard board false
        
        let isWinner = (isWinner board)
        
//        let isTie = (isTie board)
        
        match (isWinner) with
        | true -> printfn "Game Over. We are done here."
                  printBoard board true
        | false -> _playGame board (opposite playerTurn)
        
        
    _playGame board 'O'
    
    0 // return an integer exit code
