// Learn more about F# at http://fsharp.org

open System

let makeBoard numRows numCols =
     Array2D.init<char> numRows numCols (fun row col -> 'x')

//https://en.wikibooks.org/wiki/F_Sharp_Programming/Arrays
let printBoard board =
    let maxY = (Array2D.length1 board) - 1
    let maxX = (Array2D.length2 board) - 1
    
    for row in 0 .. maxY do
        for col in 0 .. maxX do
            printf "%A" board.[row, col] 
        Console.WriteLine()
    

[<EntryPoint>]
let main argv =
    printfn "Connect 4. Prepare to have fun."
    
    let NumRows = 6
    let NumCols = 7
    
    let board = makeBoard NumRows NumCols
    
    printBoard board
    
    let rec _playGame board playerTurn =
        match playerTurn with
        | 'X' -> playerXMakeMove()
        | 'Y' -> playerYMakeMove()
        
        let gameOver = isGameOver board
        match gameOver with
        | true -> printfn "Game Over. We are done here."
        | false -> ()
        
        printBoard board
        
        
    
    0 // return an integer exit code
