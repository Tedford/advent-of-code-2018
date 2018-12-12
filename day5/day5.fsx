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


let remainder =
    chars
    |> Seq.toArray
    |> reduce 
    |> Seq.choose id
    |> String.Concat
    
printfn "Remaining chain length: %d" (Seq.length remainder)

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

//printfn "initial length %d" (Seq.length chars)
        
type ReducedPolymerChain = {
    Unit: char;
    Length: int;
}

let chains = 
    ['a' .. 'z']
    |> Seq.map (fun u -> {Unit = u; Length = computeReducedChain chars u})
    |> Seq.sortBy (fun r -> r.Length)
    |> Seq.toArray
    
//chains.Dump()

let shortest = chains |> Seq.head
printfn "The shorted chain is from removing %A at a length of %d" shortest.Unit shortest.Length