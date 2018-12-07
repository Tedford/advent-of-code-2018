open System
open System.IO
open System.Text.RegularExpressions

let material = Array2D.create 1000 1000 0

type Claim = {
    Id : int;
    Xi : int;
    Yi : int;
    Xn : int;
    Yn : int;
}

let (|ClaimMatch|_|) input =
    let m = Regex.Match(input,@"#(\d+)\s+@\s+(\d+),(\d+):\s+(\d+)x(\d+)", RegexOptions.Compiled)
    match m.Success with 
    | true -> Some [for i in m.Groups -> i]
    | _ -> None
    

let parseClaim raw =
    match raw with
    | ClaimMatch [_; id; x; y; length; width] -> Some({Id = id.Value |> int; Xi = x.Value |> int; Yi = y.Value |> int; Xn = (x.Value |> int) + (length.Value |> int); Yn = (y.Value |> int) + (width.Value |> int)})
    | _ -> None

// map out the utilized squares
File.ReadAllLines(@"c:\projects\github\advent-of-code-2018\day3\input.dat")
    |> Seq.map parseClaim
    |> Seq.iter (fun claim -> 
        match claim with
        | Some c->  
            for x in c.Xi .. c.Xn - 1 do
                for y in c.Yi .. c.Yn - 1 do
                    material.[x,y] <- material.[x,y] + 1
        | _ -> ()
    )

// calculate the utilized squares
let mutable state = 0

for x in 0 .. Array2D.length1 material - 1 do
    for y in 0 .. Array2D.length2 material - 1 do
        match material.[x,y] with
        | 0 -> ()
        | 1 -> ()
        | _ -> state <- state + 1
               ()
            

printfn "Overutilized material %d" state


