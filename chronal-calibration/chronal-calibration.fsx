open System.IO
open System.Collections.Generic



let steps = 
    seq {
        use reader = new StreamReader(@"c:\projects\github\advent-of-code-2018\chronal-calibration\input.dat")
        while not reader.EndOfStream do
            yield reader.ReadLine()
    }
    |> Seq.map int


let singlePassAnalaysis steps = Seq.fold (fun acc step -> acc + step ) 0 steps
        
printfn "Single Frequency Analysis: %d" (steps |> singlePassAnalaysis)

let repeatFrequencyAnalysis (steps: int list) =
    let seen = Dictionary<int,int>()
       
    let anal list =
        let length = List.length list
        
        let rec anal2 acc index (list: int list) =
          
            match seen.ContainsKey(acc) with
            | true -> acc
            | false -> 
                seen.Add(acc,acc)
                let next = acc + list.[index]
                let index' = (index + 1) % length
                anal2 next index' list

        anal2 0 0 list
        
    anal steps
           
        
    
printfn "Repeat Frequency Analysis: %d" (steps |> Seq.toList |> repeatFrequencyAnalysis)