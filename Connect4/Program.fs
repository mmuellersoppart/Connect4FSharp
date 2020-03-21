//Marlon Mueller-Soppart
//20200319
//ob - connect 4

open System

//configurable settings
let NumRows = 6
let NumCols = 7
let NumToWin = 4

let makeBoard =
     Array2D.init<char> NumRows NumCols (fun _ _ -> ' ')

let printDivider num =
    let rec _printDivider numOfCols =
        match numOfCols with
        | 0 -> ()
        | _ -> printf "----"
               _printDivider (numOfCols - 1)
    _printDivider (NumCols - 1)
    printfn "-----"

//https://en.wikibooks.org/wiki/F_Sharp_Programming/Arrays
let printBoard board isOver =
    
    let maxY = (Array2D.length1 board) - 1
    let maxX = (Array2D.length2 board) - 1
    
    match isOver with
    | true -> for row in 0 .. maxY do
                printDivider 4
                for col in 0 .. maxX do    
                    printf "| "
                    printf "%c" ' '
                    printf " "
                printfn "|"
                
    | false -> for row in 0 .. maxY do
                 printDivider 4
                 for col in 0 .. maxX do
                    printf "| "
                    printf "%c" board.[row, col]
                    printf " "
                 printfn "|"
                
    match isOver with
    | false -> printDivider 4
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
    
    let _badInput msg =
        printfn msg
        Console.WriteLine()
        printBoard board false
        playerMakeMove board player
    
    let input = Console.ReadLine()
    match input with
    | input when String.IsNullOrEmpty input -> _badInput "submitted nothing. try again."
    | input when not (Char.IsDigit input.[0]) -> _badInput "first input not column index. try again." 
    | input ->  let inline charToInt c = int c - int '0' //https://stackoverflow.com/questions/42820232/f-convert-a-char-to-int
                let targetCol = input.[0] |> charToInt         
                if targetCol >=  NumCols then _badInput "Illegal move. try again."
                elif board.[0, targetCol] = 'O' || board.[0, targetCol] = 'X' then _badInput "column full. try again."                                                                      
                else addCharToCol board player targetCol
                ()
    ()
                                                                      
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
    //ob - produce tuples that match to all possible board coordinates (and more! (wastefully))
    let allValues = [0..(max - 1)]
    
    let rec _getTuplesGivenHd allValuesForY givenHd acc =
        match allValuesForY with
        | [] -> acc
        | hd::tl -> 
                    _getTuplesGivenHd tl givenHd ((givenHd, hd) :: acc) 
        
    let rec _allCombinations allValuesForX acc =
        match allValuesForX with
        | [] -> acc
        | hd::tl -> _allCombinations tl (_getTuplesGivenHd allValues hd [] @ acc)

    _allCombinations allValues []

let getAllDiagonalForward (board: char [,]) =
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
    
let isTie board =
    
    let matrixWidth = (Array2D.length2 board) //the length of a row (left and right)     
    let colIndexList = [0..(matrixWidth - 1)] //index of each col (left and right)
    let topRow = getRow board colIndexList 0 []
    
    let rec _isTie row =
        match row with
        | [] -> printfn "A tie. You both LOSE."
                true
        | hd::_ when hd <> 'X' && hd <> 'O' -> false
        | _::tl -> _isTie tl
        
    _isTie topRow
        

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
        
        let isTie = (isTie board)
        
        match (isWinner || isTie) with
        | true -> printfn "Game Over. We are done here."
                  printBoard board true
        | false -> _playGame board (opposite playerTurn)
        
        
    _playGame board 'O'
    
    0 // return an integer exit code
