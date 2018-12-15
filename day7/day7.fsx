open System
open System.IO

type Given  = {
    Id: string;
    Requires: string seq;
    Cost: int
}

let (|Match|_|) pattern input =
    let m = Regex.Match(input, pattern, RegexOptions.Compiled)
    match m.Success with
    | true -> Some[for i in m.Groups->i]
    | _ -> None

let parseRow row =
    match row with
    | Match @"Step (\w+) must be finished before step (\w+) can begin." [_;predecessor; successor] -> (predecessor.Value,successor.Value)
    | _ -> failwith "Unexpected input format"

//let givens = [File.ReadAllLines(@"c:\projects\github\advent-of-code-2018\day7\example.dat") |> Seq.map parseRow |> Seq.groupBy fst |> Seq.map (fun (p,s)->{Id = p; Requires = s |> Seq.map (fun (k,v) -> {Id = v; Requires = []}) |> Seq.toList } )]
//let givens = [File.ReadAllLines(@"c:\projects\github\advent-of-code-2018\day7\example.dat") |> Seq.map parseRow |> Seq.groupBy fst |> Seq.map (fun (p,s)->{Id = p; Requires = s |> Seq.map snd |> Seq.toList} )] 
//let givens = File.ReadAllLines(@"c:\projects\github\advent-of-code-2018\day7\input.dat") |> Seq.map parseRow |> Seq.groupBy fst |> Seq.map (fun (k,v) -> (k, v|> Seq.map snd))|> Map.ofSeq
let raw = File.ReadAllLines(@"c:\projects\github\advent-of-code-2018\day7\example.dat") |> Array.map parseRow 

let costs =
    raw
    |> Seq.map snd
    |> Seq.groupBy id
    |> Seq.map (fun (k,l)-> (k, Seq.length l))
    |> Map.ofSeq

let getCost state = match costs.TryFind state with | Some cost -> cost | _ -> 0

let dependencies = raw  |> Seq.groupBy fst |> Seq.map (fun (k,v) -> (k, v |> Seq.map snd )) |> Map.ofSeq

let getDependencies state = match dependencies.TryFind state with | Some x -> x |> Seq.sort | _ -> Seq.empty

let states = 
    raw
    |> Seq.collect (fun (p,s) -> seq {yield p; yield s})
    |> Seq.distinct
//    |> Seq.map (fun id -> {Id = id; Cost = getCost id; Requires = getDependencies id})
    |> Seq.map (fun id -> {Id = id; Cost = getCost id; Requires = getDependencies id})
    |> Seq.sortBy (fun s -> s.Cost, s.Id)
    |> Seq.toArray

printfn "\nStates:"
states|> Seq.iter (fun s -> printfn "\tID: %s Cost: %d Requirements: %d %A" s.Id s.Cost (Seq.length s.Requires) s.Requires)

let order = 
    states 
    |> Seq.groupBy (fun g -> g.Cost)
    |> Seq.collect (fun (_,v) -> v |> Seq.map (fun g -> g.Id) |> Seq.sort)
    |> String.Concat

// ACEUBDLSMNXZIKWYQTFGPVHJRO incorrect
printfn "\nOrder: %s" order


(*
printfn "\nGivens:"
givens.Dump()

let states = 
    givens
    |> Map.toArray
    |> Seq.collect (fun (k, v) -> [k] @ (v|> Seq.toList))
    |> Seq.distinct
    |> Seq.sort
    

printfn "\nDetected States:"
states |> Seq.iter (fun s -> printfn "\t%s" s)

let cost = 
    states
    |> Seq.map(fun s -> givens.TryFind s)
    |> Seq.choose id
    |> Seq.collect id
    |> Seq.groupBy id
    |> Seq.map(fun (k,l)-> (k,Seq.length l))
    |> Map.ofSeq


cost.Dump()
*)


//givens
//|> Seq.fold ( fun graph given ->
//    match graph with 
//    | [] -> {Id = given.Predecessor; Requires = [{Id = given.Successor; Requires = []}]}
//    | x::xs ->
//        match x.Id with
//        | given.Predecessor -> 
//        ) []