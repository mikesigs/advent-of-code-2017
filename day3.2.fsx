[<StructuredFormatDisplay("[{Index}]: {Coord} = {Value}")>]
type Cell = {
    Coord: int * int
    Index: int
    Value: int
}

type Direction = Up | Down | Left | Right

let turnLeft = function
| Up -> Left
| Left -> Down
| Down -> Right
| Right -> Up

let getNeighbours (origin: Cell) (cells: Cell list) =
    printfn "[ENTER] getNeighbours for cell %A from cells %A" origin cells
    let neighbours = cells |> List.where (fun (c: Cell) -> 
        let x2 = fst c.Coord
        let y2 = snd c.Coord
        let x1 = fst origin.Coord
        let y1 = snd origin.Coord
        let xDelta = x2 - x1 |> abs
        let yDelta = y2 - y1 |> abs
        xDelta <= 1 && yDelta <= 1 && not (xDelta = 0 && yDelta = 0))
    printfn "[EXIT]  getNeighbours for cell %A found cells %A" origin neighbours
    neighbours

let drawArc (dist: int) (direction: Direction) (cells: Cell list) : Cell list =
    let rec createCells remainingDist direction cells prevCell : Cell list =
        if remainingDist = 0 then 
            printfn "[EXIT] createCells with cells %A" cells
            cells
        else
            printfn "[ENTER] createCells from %A in direction %A with cells %A" prevCell direction cells
            let newCell = {
                Coord = match direction with
                        | Up -> fst prevCell.Coord, snd prevCell.Coord + 1
                        | Down -> fst prevCell.Coord, snd prevCell.Coord - 1
                        | Left -> fst prevCell.Coord - 1, snd prevCell.Coord
                        | Right -> fst prevCell.Coord + 1, snd prevCell.Coord
                Index = prevCell.Index + 1
                Value = 0
            }        
            let cellWithValue = { 
                newCell with 
                    Value = getNeighbours newCell cells 
                            |> List.sumBy (fun (c: Cell) -> c.Value) 
            }
            createCells (remainingDist - 1) direction (cellWithValue :: cells) cellWithValue
    createCells dist direction cells (List.head cells)

let day3 maxIndex target =
    let rec drawSpiral (index: int) (maxIndex: int) (target: int) (distance: int) (direction: Direction) (cells: Cell list) =
        printfn "drawSpiral index: %d, direction: %A, cells: %A" index direction cells
        // Exit condition for Part 1
        if index + distance >= maxIndex then    
            drawArc (abs maxIndex - index) direction cells
        else 
            let newCells = drawArc distance direction cells
            // Exit condition for Part 2
            if (newCells |> List.head |> (fun (c: Cell) -> c.Value > target)) then
                newCells
            else  
                let newDir = turnLeft direction
                let newDist = 
                    match newDir with
                    | Left | Right -> distance + 1
                    | Up | Down -> distance
                    
                drawSpiral (index + distance) maxIndex target newDist newDir newCells
    
    let seed = [{ Coord = (0,0); Index = 1; Value = 1 }]
    drawSpiral 1 maxIndex target 1 Right seed    

// Part 1
day3 25 100000 |> List.head |> fun (c: Cell) -> abs (fst c.Coord) + abs (snd c.Coord)

// Part 2
day3 1000 289326
