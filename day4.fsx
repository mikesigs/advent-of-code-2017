// Part 1
let day4 (input: seq<string>) = 
    (input |> Seq.distinct |> Seq.length) = (Seq.length input)

let split (by: char) (input: string) = input.Split(by) |> Seq.ofArray
let splitWords = split ' '

System.IO.File.ReadLines "data/day4.txt"
|> Seq.map (splitWords >> day4) 
|> Seq.fold (fun count e -> if e then count + 1 else count) 0 //=> 337