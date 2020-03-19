// Learn more about F# at http://fsharp.org

open System

let NumRows = 6
let NumCols = 7
let NumToWin = 4

let makeBoard =
     Array2D.init<char> NumRows NumCols (fun row col -> ' ')

//https://en.wikibooks.org/wiki/F_Sharp_Programming/Arrays
let printBoard board =
    let maxY = (Array2D.length1 board) - 1
    let maxX = (Array2D.length2 board) - 1
    
    for row in 0 .. maxY do
        for col in 0 .. maxX do
            printf "%A" board.[row, col] 
        Console.WriteLine()

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
                                              printBoard board
                                              playerMakeMove board player
                                              
    if board.[0, targetCol] = 'O' || board.[0, targetCol] = 'X' then printfn "column full. try again."
                                                                     Console.WriteLine()
                                                                     printBoard board
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
    
    printBoard board
    
    
    
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
        
        printBoard board
        
    _playGame board 'O'
    
    0 // return an integer exit code
