type Coord = (int * int)
type dir = Up | Down | Left | Right
let moveUp (c: Coord) (d: int) = fst c, snd c + d
let moveDown (c: Coord) (d: int) = fst c, snd c - d
let moveLeft (c: Coord) (d: int) = fst c - d, snd c
let moveRight (c: Coord) (d: int) = fst c + d, snd c
let move (dist: int) (dir: dir) (pos: int * int) =
    printfn "dist: %d, dir: %A, pos: %A" dist dir pos

    let newpos = 
        match dir with
        | Up -> moveUp pos dist
        | Down -> moveDown pos dist
        | Left -> moveLeft pos dist
        | Right -> moveRight pos dist
    
    newpos

let turnLeft = function
| Up -> Left
| Left -> Down
| Down -> Right
| Right -> Up

let turnAround = turnLeft >> turnLeft

let draw max =    
    let rec step i max dist dir pos =
        if i > max then pos
        else 
            pos
            |> move dist dir
            |> move dist (turnLeft dir)
            |> step (i + dist*2) max (dist+1) (turnAround dir)

    step 1 max 1 Right (0,0)

draw 25