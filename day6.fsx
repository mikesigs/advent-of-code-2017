let input = [|5;1;10;0;1;7;13;14;3;12;8;10;7;12;0;6|]


let splice index fn (arr: array<int>) =
    let mid = arr.[index]
    let left = if index = 0
               then [||] 
               else arr.[..index-1]
    let right = if index = arr.Length - 1
                then [||] 
                else arr.[index+1..]
    Array.concat [left; [|fn mid|]; right]

let inc = (+) 1

let rec divvy amount prevIndex ints =
    // printfn "divvy: amount = %d, index = %d, ints = %A" amount prevIndex ints
    if amount = 0 
    then ints
    else 
        let index = if prevIndex + 1 = (Array.length ints) then 0 else prevIndex + 1
        divvy (amount - 1) index (splice index inc ints)

let hash = Array.fold (fun acc x -> acc + (sprintf "|%i" x)) ""

let rec day6 c (prev: Set<string>) ints =
    let max = Array.max ints
    let maxIndex = Array.findIndex ((=) max) ints 
    let newInts = divvy max maxIndex (splice maxIndex ((-) max) ints)
    let newHash = hash newInts
    //printfn "day6: max = %d, maxIndex = %d, newInts = %A, newHash = %s, prev = %A" max maxIndex newInts newHash prev
    if (Set.contains newHash prev) then c+1
    else day6 (c+1) (Set.add newHash prev) newInts

day6 0 Set.empty input //[|0;2;7;0;|]



