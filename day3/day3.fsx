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
    Area: int;
}

let (|ClaimMatch|_|) input =
    let m = Regex.Match(input,@"#(\d+)\s+@\s+(\d+),(\d+):\s+(\d+)x(\d+)", RegexOptions.Compiled)
    match m.Success with 
    | true -> Some [for i in m.Groups -> i]
    | _ -> None
    

let parseClaim raw =
    match raw with
    | ClaimMatch [_; id; x; y; length; width] -> 
        let x' = x.Value |> int
        let y' = y.Value |> int
        let length' = length.Value |> int
        let width' = width.Value |> int
        Some({Id = id.Value |> int; Xi = x'; Yi = y'; Xn = x' + length' - 1; Yn = y' + width' - 1; Area = length' * width' })
    | _ -> None

let claims = File.ReadAllLines(@"c:\projects\github\advent-of-code-2018\day3\input.dat") |> Seq.map parseClaim |> Seq.choose id

// map out claims
claims    
|> Seq.iter (fun c -> 
        for x in c.Xi .. c.Xn do
            for y in c.Yi .. c.Yn do
                material.[x,y] <- material.[x,y] + 1
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


let checkForOverlap (m: int[,]) c =
    let mutable state = 0
    
    m.[c.Xi .. c.Xn, c.Yi .. c.Yn]
    |> Array2D.iter (fun e -> state <- state + e)
    
    state = c.Area |> not
    
let checkMaterial (c:Claim) = checkForOverlap material c |> not

claims |> Seq.filter checkMaterial |> Seq.iter ( fun c -> printfn "Allowed claim %A" c)
