open System
open System.Collections.Generic
open System.IO

let reacts c1 c2 = 
    match c1, c2 with
    | None,  _ -> false
    | _ , None -> false
    | Some c1', Some c2' -> Char.ToLower(c1') = Char.ToLower(c2') && Char.ToUpper(c1') = Char.ToUpper(c2') && c1' <> c2'

let markedForDeletion = function None -> false | _ -> true

let chars = 
        File.ReadAllBytes(@"c:\projects\github\advent-of-code-2018\day5\input.dat")
        |> Seq.map (fun b-> b |> char |> Some)
        |> Seq.toArray

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
    reduce chars
    |> Seq.choose id
    |> String.Concat
    
printfn "Remaining chain length: %d" (Seq.length remainder)
