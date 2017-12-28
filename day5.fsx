let rec day5 (c: int) (index: int) (inc: int -> int) (input: array<int>) =
    if index > input.Length - 1 
    then c
    else  
        let jmp = input.[index]
        let left = if index-1 < 0 
                   then [||] 
                   else input.[..index-1]
        let right = if index+1 > input.Length 
                    then [||] 
                    else input.[index+1..]
        day5 (c + 1) (index + jmp) inc (Array.concat [ left; [| inc jmp |]; right ])

let input = 
    System.IO.File.ReadAllLines "data/day5.txt" 
    |> Array.map int

// Part 1
input |> day5 0 0 ((+) 1)

// Part 2
input |> day5 0 0 (fun x -> if x >= 3 then x - 1 else x + 1)