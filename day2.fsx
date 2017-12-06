// Part 1
System.IO.File.ReadLines "data/day2.1-example.csv"
|> Seq.sumBy (fun line -> 
    line.Split('\t') 
    |> Seq.map int  
    |> fun e -> Seq.min e, Seq.max e
    |> fun e -> snd e - fst e)

// Part 2
let cartesian xs ys =
    xs |> Seq.collect (fun x -> ys |> Seq.map (fun y -> x, y))

System.IO.File.ReadLines "data/day2.csv"
|> Seq.sumBy(fun line -> 
    line.Split('\t')
    |> Seq.map int
    |> (fun nums -> cartesian nums nums)
    |> Seq.pick (fun (x,y) -> if x<>y && y%x = 0 then Some(x,y) else None)
    |> fun e -> snd e / fst e)
    