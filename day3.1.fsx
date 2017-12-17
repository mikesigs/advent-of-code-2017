type Coord = (int * int)
type Dir = Up | Down | Left | Right
let moveUp (c: Coord) (d: int) = fst c, snd c + d
let moveDown (c: Coord) (d: int) = fst c, snd c - d
let moveLeft (c: Coord) (d: int) = fst c - d, snd c
let moveRight (c: Coord) (d: int) = fst c + d, snd c
let move (dist: int) (dir: Dir) (pos: int * int) =
    printfn "dist: %d, dir: %A, pos: %A" dist dir pos
    match dir with
    | Up -> moveUp pos dist
    | Down -> moveDown pos dist
    | Left -> moveLeft pos dist
    | Right -> moveRight pos dist    

let turnLeft = function
| Up -> Left
| Left -> Down
| Down -> Right
| Right -> Up

let turnRight = turnLeft >> turnLeft >> turnLeft

let findTargetPos target =    
    let rec step i target dist dir pos =
        if i >= target then    
            printfn "pos: %A, target: %d, i: %d, dir: %A" pos target i dir
            move (abs target-i) (turnRight dir) pos
        else 
            let newPos = move dist dir pos
            let newDir = turnLeft dir
            let newDist = 
                match newDir with
                | Left | Right -> dist + 1
                | Up | Down -> dist
            
            step (i + dist) target newDist newDir newPos

    step 1 target 1 Right (0,0)

findTargetPos 289326 
|> fun (x,y) -> abs x + abs y