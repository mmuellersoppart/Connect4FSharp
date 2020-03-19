// Learn more about F# at http://fsharp.org

open System

let makeBoard numRows numCols =
     Array2D.init<char> numRows numCols (fun row col -> ' ')

//https://en.wikibooks.org/wiki/F_Sharp_Programming/Arrays
let printBoard board =
    let maxY = (Array2D.length1 board) - 1
    let maxX = (Array2D.length2 board) - 1
    
    for row in 0 .. maxY do
        for col in 0 .. maxX do
            printf "%A" board.[row, col] 
        Console.WriteLine()

let addCharToCol board (player: char) (colIndex: int) =
    
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
   
let isWinner board =
    true
     

[<EntryPoint>]
let main argv =
    printfn "Connect 4. Prepare to have fun."
    
    let NumRows = 6
    let NumCols = 7
    
    let board = makeBoard NumRows NumCols
    
    addCharToCol board 'X' 0 
    addCharToCol board 'X' 0 
    addCharToCol board 'O' 0 
    addCharToCol board 'X' 1 
    
    printBoard board
    
    let rec _playGame board playerTurn =
        match playerTurn with
        | 'X' -> playerMakeMove board 'X'
        | 'O' -> playerMakeMove board 'O'
        | _ -> ()
        
        let gameOver = isWinner board
        
        match gameOver with
        | true -> printfn "Game Over. We are done here."
        | false -> _playGame board (opposite playerTurn)
        
        printBoard board
        
    _playGame board 'O'
    
    0 // return an integer exit code
