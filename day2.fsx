// Part 1
System.IO.File.ReadLines "data/day2.1-example.csv"
|> Seq.sumBy (fun line -> 
    line.Split('\t') 
    |> Seq.map int  
    |> fun e -> Seq.min e, Seq.max e
    |> fun e -> snd e - fst e)
