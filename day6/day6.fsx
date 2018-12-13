open System
open System.IO

type Assignment =
    | Unassigned
    | Common of Distance : int
    | Anchor of Id : string
    | Coordinate of Id : string * Distance : int
    | Infinite
    | Finite of Distance : int
    
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
    // transpose x & y so that the 2D array coordinates match the problem sample
    | Match @"(\d+),\s(\d+)" [_; y;x] -> { Id = assignId id; X = x.Value |> int; Y = y.Value |> int} |> Some
    | _->None
    
let printMap (xn,yn) (map:Assignment[,]) =
    printfn "\nMap"
    for x in 0 .. xn do
        for y in 0 .. yn do
            match map.[y,x] with Coordinate(id,_) -> printf "%2s " (id.ToLower()) | Anchor id -> printf "%2s " id | Infinite -> printf " 8 " | Finite _ -> printf " # " | _ -> printf " . "
        printf "\n"

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

let plane = Array2D.create (Yn + 1) (Xn + 1) Unassigned

printfn "Assigning Zones"
locations 
|> Seq.iter (fun loc ->
    for x in Xi .. Xn do
        for y in Yi .. Yn do
            let distance' = manhattanDistance (x,y) (loc.X, loc.Y)
            if distance' = 0 then printfn "\tzone %s at %dx%d" loc.Id loc.X loc.Y
            
            match distance', plane.[y,x] with
            | 0, _ -> plane.[y,x] <- Anchor(loc.Id) 
            | _,Unassigned -> plane.[y,x] <- Coordinate(loc.Id, distance')
            | _,Common distance -> 
                match relativeDistance distance distance' with
                | Closer -> plane.[y,x] <- Coordinate(loc.Id, distance')
                | _ -> ()
            | _,Coordinate (_,distance) ->
                match relativeDistance distance distance' with
                | Closer -> plane.[y,x] <- Coordinate(loc.Id, distance')
                | Equal -> plane.[y,x] <- Common(distance)
                | _ -> ()
            | _ -> ()
)

let infiniteZones = 
    seq {
        for x in Xi .. Xn do
            match plane.[Yi,x] with
            | Coordinate (id,_) -> yield id |> Some
            | _ -> yield None
            
            match plane.[Yn,x] with
            | Coordinate (id,_) -> yield id |> Some
            | _ -> yield None
            
        for y in Yi .. Yn do
            match plane.[y,Xi] with
            | Coordinate (id,_) -> yield id |> Some
            | _ -> yield None
            
            match plane.[y,Xn] with
            | Coordinate (id,_) -> yield id |> Some
            | _ -> yield None
    }
    |> Seq.choose id
    |> Seq.distinct
    |> Seq.toArray

printfn "\nInfinite Zones:"
infiniteZones
|> Seq.iter (fun z -> printfn "\t%s" z)

let threshold = 10000
printfn "\nAllowed Threshold %d" threshold

let isInfineZone (id:string) = infiniteZones |> Seq.contains id

let zones =
    seq {
        for x in Xi .. Xn do
            for y in Yi .. Yn ->
                match plane.[y,x] with
                | Coordinate (id,_) -> id |> Some
                | _ -> None}
    |> Seq.choose id
    |> Seq.groupBy id
    |> Seq.map (fun (id,occurrances) -> {Id = id; Area = (+) (Seq.length occurrances) 1; Infinte = isInfineZone id})
    |> Seq.sortByDescending (fun z -> z.Area)


let finiteZones = zones |> Seq.filter (fun z -> z.Infinte |> not) |> Seq.toArray

printfn "\nFinite Zones:"
finiteZones |> Seq.iter (fun z-> printfn "\tID: %2s Area: %4d" z.Id z.Area)

printMap (Xn,Yn) plane

let globalCost (x,y) =
    locations
    |> Seq.map (fun l -> manhattanDistance (x,y) (l.X,l.Y))
    |> Seq.sum

for x in Xi .. Xn do
    for y in Yi .. Yn do
        match plane.[y,x] with
        | Coordinate (_,_) -> plane.[y,x] <- Finite(globalCost (x,y))
        | Common _ -> plane.[y,x] <- Finite (globalCost(x,y))
        | _ -> ()


let calculateSafeArea threshold =
    seq {
    for x in Xi .. Xn do
        for y in Yi .. Yn ->
            match plane.[y,x] with
            | Finite distance when distance < threshold -> 1
            | _ -> 0
    }
    |> Seq.sum


// attempt #1 - 37282 too low
// Attempt #2 - Too low
//    Safe Area 38094
//    Safe Anchors 28
//    Total 38122
// Attempt #3 - 
//    Safe Area 46514
//    Safe Anchors 28
//    Total 46542

let safeArea = (calculateSafeArea threshold) 
let safeAnchors = locations |> Seq.map (fun loc-> globalCost (loc.X, loc.Y)) |> Seq.filter (fun cost -> cost < threshold) |> Seq.length 
printfn  "\nSafe Area %d\nSafe Anchors %d\nTotal %d" safeArea safeAnchors (safeArea + safeAnchors)

let printMapWithCost (xn,yn) threshold (map:Assignment[,]) =
    printfn "\nMap"
    for x in 0 .. xn do
        for y in 0 .. yn do
            match map.[y,x] with Coordinate(id,_) -> printf "%2s " (id.ToLower()) | Anchor id -> printf "%2s " id | Infinite -> printf " 8 " | Finite x when x < threshold -> printf " # " | _ -> printf " . "
        printf "\n"


printMapWithCost (Xn,Yn) threshold plane