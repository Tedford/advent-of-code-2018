open System

let lines = File.ReadAllLines("input.dat")
            |> Seq.toList

let analyze (box:string) =
    box.ToCharArray()
    |> Seq.groupBy id
    |> Seq.map (fun a -> fst a, snd a |> Seq.length)

let hasCount target input =
    input 
    |> Seq.filter (fun t -> (snd t) = target )
    |> Seq.length > 0
let analyzed = lines |> Seq.map analyze

let doubles =
    analyzed
    |> Seq.filter (fun t-> hasCount 2 t)
    
let triples =
    analyzed
    |> Seq.filter (fun t-> hasCount 3 t)


printfn "CheckSum: %d" ((doubles |> Seq.length) * (triples |> Seq.length))


let score (id:string) =
    let score = 
        id.ToCharArray()
        |> Seq.mapi (fun i c -> (c |> int) * i)
        |> Seq.sum
    
    id, score
    

let differences w1 w2 =
    let length = (Seq.length w1) - 1
    let w1' = Seq.toArray w1
    let w2' = Seq.toArray w2
    seq {
        for i in 0 .. length -> if w1'.[i] = w2'.[i] then 0 else 1
    } 
    |> Seq.filter (fun diff -> diff = 1)
    |> Seq.length
    
let inCommon (w1:string) (w2:string) =
    Seq.zip w1 w2
    |> Seq.filter (fun (c1, c2) -> c1 = c2)
    |> Seq.map fst
    |> String.Concat
    
let rec traverse (ids: string list) =
    match ids with
    | id :: remainder -> 
        let scores = 
            remainder
            |> List.map (fun id' -> id, id', differences id id')
            |> List.filter (fun (_, _, s) -> s = 1 )
        match scores with
        | xs :: _ -> xs
        | [] -> traverse remainder
    | [] -> failwith "Id not found"  

let (id1, id2, _) = traverse lines

printfn "Common ID: %s" (inCommon id1 id2)
