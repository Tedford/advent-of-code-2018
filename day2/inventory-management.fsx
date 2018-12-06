open System.IO
let analyze (box:string) =
    box.ToCharArray()
    |> Seq.groupBy id
    |> Seq.map (fun a -> fst a, snd a |> Seq.length)
    |> Seq.filter (fun a -> snd a > 1)

let hasCount target input =
    input 
    |> Seq.filter (fun t -> (snd t) = target )
    |> Seq.length > 0
let analyzed = lines |> Seq.map analyze
let doubles =
    analyzed
    |> Seq.filter (fun t-> hasCount 2 t)
    |> Seq.length
    
let triples =
    analyzed
    |> Seq.filter (fun t-> hasCount 3 t)
    |> Seq.length

printfn "CheckSum: %d" (doubles * triples)