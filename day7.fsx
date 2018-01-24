open System.Text.RegularExpressions
open System
open System.Linq

type Node = { name: string; children: string seq }

System.IO.File.ReadLines "data/day7.txt"
|> Seq.fold (fun acc line -> if line.Contains("->") then line :: acc else acc) []
|> Seq.map (fun line -> 
    let m = Regex.Match(line, "(?<name>\w+).*->\s((?<children>\w+),\s)+")
    { name = m.Groups.["name"].Value; children = m.Groups.["children"].Captures.Cast<Capture>().Select(fun c -> c.Value) })
|> Seq.iter (printfn "%A")