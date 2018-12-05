open System.IO

let steps = 
    seq {
        use reader = new StreamReader(@"input.dat")
        while not reader.EndOfStream do
            yield reader.ReadLine()
    }
    |> Seq.map int


let result = Seq.fold (fun acc step -> acc + step) 0 steps

printfn "%d" result