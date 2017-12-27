let rec day5 (c: int) (i: int) (input: array<int>) =
    if (i > input.Length - 1) then c
    else  
        let jmp = input.[i]
        let left = if (i-1 < 0) then [||] else input.[..i-1]
        let right = if (i+1 > input.Length) then [||] else input.[i+1..]
        day5 (c + 1) (i + jmp) (Array.concat [ left; [| jmp + 1 |]; right ])

System.IO.File.ReadAllLines "data/day5.txt" 
|> Array.map int
|> day5 0 0

day5 0 0 [| 1; -1; -2 |]

