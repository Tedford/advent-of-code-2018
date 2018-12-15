open System
open System.IO

let (|Match|_|) pattern input =
    let m = Regex.Match(input, pattern, RegexOptions.Compiled)
    match m.Success with
    | true -> Some[for i in m.Groups->i]
    | _ -> None

let merge a b =
    Map.fold (fun s k v ->
        match Map.tryFind k s with
        | Some _ -> s
        | None -> Map.add k v s) a b

let parseRow row =
    match row with
    | Match @"Step (\w+) must be finished before step (\w+) can begin." [_;predecessor; successor] -> (successor.Value,predecessor.Value)
    | _ -> failwith "Unexpected input format"

// example - CABDFE
// example 2 - AFGYZ
let raw = File.ReadAllLines(@"c:\projects\github\advent-of-code-2018\day7\input.dat") |> Array.map parseRow 

// all of the system states
let states = raw |> Array.toList |> List.collect (fun (s,p) -> [yield p; yield s]) |> List.distinct |> List.sort

// a record of which states have been built
let built = states |> Seq.map (fun id -> (id,false)) |> Map.ofSeq

// a mapping of each state and its prerequsites
let dependencies = 
    merge 
        (raw |> Seq.groupBy fst |> Seq.map (fun (k,v)-> (k,  v |> Seq.map snd |> Seq.toArray )) |> Map.ofSeq)
        (states |> List.map (fun s -> (s,Array.empty<string>))|> Map.ofList)

printfn "Dependencies Chain"
dependencies |> Map.iter (fun k -> printfn "\t%s := %A" k)

let rec nextState states (built: Map<string,bool>) =
    match states with
    | x :: xs -> 
        match built.[x] with
        | true -> nextState xs built
        | false ->
            let d= dependencies.[x]
            match Seq.length d with
            | 0 -> x
            | _ -> match d |> Seq.map (fun p -> if built.[p] then 1 else 0) |> Seq.sum = Seq.length d with true -> x | false -> nextState xs built
    | _ -> failwith "All states have been built"


Seq.unfold (fun (s,h) -> 
    match s with 
    | [] -> None
    | _ ->
        let next = nextState s h
        let history' = Map.add next true h
        let toBuild' = s |> List.filter (fun p -> p <> next)
        Some((next, Map.empty<string,bool>), (toBuild',history'))
) (states, built)
|> Seq.map fst
|> String.Concat
|> printfn "\nWorkflow: %s" 