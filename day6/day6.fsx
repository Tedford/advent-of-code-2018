open System
open System.IO

type Assignment =
    | Unassigned
    | Common of Distance : int
    | Coordinate of Id : string * Distance : int

type Distance = 
    | Closer
    | Equal
    | Further

type Location = {
    Id: string;
    X: int;
    Y: int;
}

type Zone = {
    Id: string;
    Area: int;
    Infinte: bool;
}

let (|Match|_|) pattern input =
    let m = Regex.Match(input, pattern, RegexOptions.Compiled)
    match m.Success with
    | true -> Some[for i in m.Groups->i]
    | _ -> None
    
let assignId id =
    (if id > 25 then ('A' + ((id / 26 - 1) |> char)).ToString() else "") + ('A' + (id % 26 |> char)).ToString()
    

let parseRow id row =
    match row with
    | Match @"(\d+),\s(\d+)" [_; x; y] -> { Id = assignId id; X = x.Value |> int; Y = y.Value |> int} |> Some
    | _->None

let locations =
    File.ReadAllLines(@"c:\projects\github\advent-of-code-2018\day6\input.dat")
    |> Seq.mapi parseRow
    |> Seq.choose id

let manhattanDistance (x1:int,y1:int) (x0,y0) = Math.Abs(x1-x0) + Math.Abs(y1-y0)

let relativeDistance d1 d2 =
    match d1 = d2, d1 < d2, d1 > d2 with
    | true, _, _ -> Equal
    | _,  true, _ -> Further
    | _, _, true -> Closer
    | _ -> failwith "invalid case"
    
let Xi = locations |> Seq.map (fun r -> r.X) |> Seq.min
let Yi = locations |> Seq.map (fun r -> r.Y) |> Seq.min
let Xn = locations |> Seq.map (fun r -> r.X) |> Seq.max
let Yn = locations |> Seq.map (fun r -> r.Y) |> Seq.max

printfn "Bounding Box\n\t%dx%d to %dx%d\n" Xi Yi Xn Yn

let plane = Array2D.create (Xn + 1) (Yn + 1) Unassigned

printfn "Assigning Zones"
locations 
|> Seq.iter (fun loc ->
    for x in Xi .. Xn do
        for y in Yi .. Yn do
            let distance' = manhattanDistance (x,y) (loc.X, loc.Y)
            let id = 
                if distance' = 0 then 
                    printfn "\tzone %s at %dx%d" loc.Id loc.X loc.Y
                    loc.Id.ToUpper()
                else
                    loc.Id.ToLower()
            match plane.[x,y] with
            | Unassigned -> plane.[x,y] <- Coordinate(id, distance')
            | Common distance -> 
                match relativeDistance distance distance' with
                | Closer -> plane.[x,y] <- Coordinate(id, distance')
                | _ -> ()
            | Coordinate (_,distance) ->
                match relativeDistance distance distance' with
                | Closer -> plane.[x,y] <- Coordinate(id, distance')
                | Equal -> plane.[x,y] <- Common(distance)
                | _ -> ()
)

let infiniteZones = 
    seq {
        for x in Xi .. Xn do
            match plane.[x,Yi] with
            | Coordinate (id,_) -> yield id.ToUpper() |> Some
            | _ -> yield None
            
            match plane.[x,Yn] with
            | Coordinate (id,_) -> yield id.ToUpper() |> Some
            | _ -> yield None
     
            
        for y in Yi .. Yn do
            match plane.[Xi,y] with
            | Coordinate (id,_) -> yield id.ToUpper() |> Some
            | _ -> yield None
            
            match plane.[Xn,y] with
            | Coordinate (id,_) -> yield id.ToUpper() |> Some
            | _ -> yield None
    }
    |> Seq.choose id
    |> Seq.distinct
    |> Seq.toArray

printfn "\nInfinite Zones:"
infiniteZones
|> Seq.iter (fun z -> printfn "\t%s" z)

let isInfineZone (id:string) = infiniteZones |> Seq.contains id |> not

let zones =
    seq {
        for x in Xi .. Xn do
            for y in Yi .. Yn ->
                match plane.[x,y] with
                | Coordinate (id,_) -> id.ToUpper() |> Some
                | _ -> None}
    |> Seq.choose id
    |> Seq.groupBy id
    |> Seq.map (fun (id,occurrances) -> {Id = id; Area = Seq.length occurrances; Infinte = isInfineZone id |> not})
    |> Seq.sortByDescending (fun z -> z.Area)

printfn "\nFinite Zones:"
zones |> Seq.filter (fun z -> z.Infinte |> not) |> Seq.iter (fun z-> printfn "\tID: %2s Area: %4d" z.Id z.Area)


//for x in Xi .. Xn do
//    for y in Yi .. Yn do
//        match plane.[x,y] with Coordinate(id,_) -> printf "%2s " id | Common _ -> printf " . " | _ -> ()
//    printf "\n"