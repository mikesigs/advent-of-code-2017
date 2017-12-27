let rec day5 (c: int) (i: int) (inc: int -> int) (input: array<int>) =
    if (i > input.Length - 1) then c
    else  
        let jmp = input.[i]
        let left = if (i-1 < 0) then [||] else input.[..i-1]
        let right = if (i+1 > input.Length) then [||] else input.[i+1..]
        day5 (c + 1) (i + jmp) inc (Array.concat [ left; [| inc jmp |]; right ])

let input = 
    System.IO.File.ReadAllLines "data/day5.txt" 
    |> Array.map int

// Part 1
input |> day5 0 0 ((+) 1)

// Part 2
input |> day5 0 0 (fun x -> if x >= 3 then x - 1 else x + 1)