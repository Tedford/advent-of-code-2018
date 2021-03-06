open System
open System.Collections.Generic
open System.IO
open System.Diagnostics

let reacts c1 c2 = 
    match c1, c2 with
    | None,  _ -> false
    | _ , None -> false
    | Some c1', Some c2' -> Char.ToLower(c1') = Char.ToLower(c2') && Char.ToUpper(c1') = Char.ToUpper(c2') && c1' <> c2'

let markedForDeletion = function None -> false | _ -> true

let chars = 
    File.ReadAllBytes(@"c:\projects\github\advent-of-code-2018\day5\input.dat")
    |> Seq.map (fun b-> b |> char |> Some)

printfn "Initial chain length: %d" (Seq.length chars)

let CheckIfInert (polymerChain: char option array) =
    let changes = 
        [for i in 1 .. Array.length polymerChain - 1 ->
            match reacts polymerChain.[i-1] polymerChain.[i] with
            | true ->
                polymerChain.[i-1] <- None
                polymerChain.[i] <- None
                1
            | _ -> 0]
        |> Seq.sum
    changes = 0

let rec reduce polymerChain =
    match CheckIfInert polymerChain with
    | true -> polymerChain
    | false -> polymerChain |> Array.filter markedForDeletion |> reduce 

let watch = Stopwatch.StartNew()
let remainder =
    chars
    |> Seq.toArray
    |> reduce 
    |> Seq.choose id
    |> String.Concat
watch.Stop()
    
printfn "Remaining chain length: %d determined in %A" (Seq.length remainder) watch.Elapsed
        
type ReducedPolymerChain = {
    Unit: char;
    Length: int;
}

let removeUnit unit polymerChain =
    for i in 0 .. Array.length polymerChain - 1 do
        match polymerChain.[i] with
        | Some x when x = Char.ToLower(unit) || x = Char.ToUpper(unit) -> polymerChain.[i] <- None
        | _ -> ()
    polymerChain
        
let computeReducedChain polymerChain unit =
    polymerChain 
    |> Seq.toArray
    |> removeUnit unit
    |> reduce
    |> Seq.choose id
    |> Seq.length
    
let asyncComputeReducedChain unit =
    async { 
        let watch = Stopwatch.StartNew()
        let length = computeReducedChain chars unit
        watch.Stop()
        printfn "\tComputed reduced chain %A in %A" unit watch.Elapsed
        return {Unit = unit; Length = length}
    }

//printfn "initial length %d" (Seq.length chars)

let chains = 
    ['a' .. 'z']
    |> Seq.map asyncComputeReducedChain
    |> Async.Parallel
    |> Async.RunSynchronously
    |> Seq.sortBy (fun r -> r.Length)    
    |> Seq.toArray

let shortest = chains |> Seq.head
printfn "The shorted chain is from removing %A at a length of %d" shortest.Unit shortest.Length