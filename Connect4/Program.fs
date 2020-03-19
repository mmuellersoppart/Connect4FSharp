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
        | currRow when currRow >= (Array2D.length2 board - 1) -> true
        | currRow when (board.[currRow, colIndex] = 'X') || (board.[currRow, colIndex] = 'O') -> true
        | currRow when (_addCharToCol board player colIndex (currRow + 1)) -> (board.[currRow, colIndex] <- player)
                                                                              false
        | _ -> false
        
    _addCharToCol board player colIndex 0 |> ignore
    ()

let rec playerMakeMove board player=
    Console.WriteLine()
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

let printWinMsg player =
    printf "Player "
    printf "%c" player
    printfn " Won. Yay."

let checkForWinnerInList L =
    let streak = 0
    let rec _checkForWinnerInList L streak player =
        match L with
        | [] -> false
        | hd::tl when hd = ' ' -> _checkForWinnerInList tl 0 player
        | hd::tl when hd = player && (streak + 1) = NumToWin -> printWinMsg player
                                                                true
        | hd::tl when hd = player -> _checkForWinnerInList tl (streak + 1) (player)
        | hd::tl when hd <> player -> _checkForWinnerInList tl 1 (hd)
    
    _checkForWinnerInList L 0 'X'
    
        

let isWinner board numToWin =
    true
     

[<EntryPoint>]
let main argv =
    printfn "Connect 4. Prepare to have fun."
    
    let board = makeBoard
    
    addCharToCol board 'X' 0
    addCharToCol board 'O' 0
    addCharToCol board 'O' 0
    addCharToCol board 'O' 0

    printfn "%A" "sup "
    printfn "%A" (getCol board [0..5] 0 [])
    
    printBoard board false
//    printBoard board true
    
    
    
    printfn "%A" (getAllCols board)
    
    let rec _playGame board playerTurn =
        match playerTurn with
        | 'X' -> playerMakeMove board 'X'
        | 'O' -> playerMakeMove board 'O'
        | _ -> ()
        
        let gameOver = isWinner board NumToWin
        
        match gameOver with
        | true -> printfn "Game Over. We are done here."
        | false -> _playGame board (opposite playerTurn)
        
        printBoard board true
        
    _playGame board 'O'
    
    0 // return an integer exit code
