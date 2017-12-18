// Return the input with its characters in alphabetical order
let alphabetize (input: string) : string = input |> Seq.toList |> Seq.sort |> System.String.Concat
let getWords (input: string) = input.Split(' ') |> Seq.ofArray
let countTrues s = Seq.fold (fun count e -> if e then count + 1 else count) 0 s

let day4 projection input = (input |> Seq.distinctBy projection |> Seq.length) = (Seq.length input)
let day4part1: seq<string> -> bool = day4 id
let day4part2: seq<string> -> bool = day4 alphabetize

let lines = System.IO.File.ReadLines "data/day4.txt"

// Part 1
lines |> Seq.map (getWords >> day4part1) |> countTrues //=> 337

// Part 2
lines |> Seq.map (getWords >> day4part2) |> countTrues //=> 231