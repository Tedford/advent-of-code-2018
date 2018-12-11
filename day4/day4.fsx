open System
open System.IO
open System.Text.RegularExpressions

type Activity =
    | Wakes of Time: DateTime
    | Sleeps of Time: DateTime
    | OnDuty of Time: DateTime * Id: int
    
 type ShiftRecord = {
    Id: int;
    Started: DateTime;
    Activity: Activity list;    
}

type SleepRecord = {
    Id: int;
    Duration: int;
}
    
let (|Match|_|) pattern input =
    let m = Regex.Match(input, pattern, RegexOptions.Compiled)
    match m.Success with
    | true -> Some [for i in m.Groups -> i]
    | _ -> None

(*
[1518-10-03 00:47] falls asleep
[1518-07-26 23:50] Guard #487 begins shift
[1518-06-22 00:48] wakes up
*)
let parseRow line =
    match line with
    | Match @"\[(\d+-\d+-\d+\s\d+:\d+)]\sfalls\sasleep" [_;timestamp] -> Sleeps(DateTime.Parse(timestamp.Value)) |> Some
    | Match @"\[(\d+-\d+-\d+\s\d+:\d+)]\swakes\sup" [_;timestamp] -> Wakes(DateTime.Parse(timestamp.Value)) |> Some
    | Match @"\[(\d+-\d+-\d+\s\d+:\d+)]\sGuard #(\d+)" [_;timestamp;id] ->OnDuty(DateTime.Parse(timestamp.Value), id.Value |> int) |> Some
    | _ -> None


let groupActivityByDay (history: seq<_>)  =
    let enumerator = history.GetEnumerator()

    // list comprehension as the enumerator will be empty on retraversal
    [
        let mutable activity = []
        
        while enumerator.MoveNext() do
            match enumerator.Current with
                | OnDuty(_,_) ->
                    yield activity
                    activity <- [enumerator.Current] 
                | _ -> activity <- activity @ [enumerator.Current] 
        yield activity
    ]
    |> Seq.map (fun r -> 
        match r with
        | (OnDuty (timestamp,id))::xs -> 
            { Id = id; Started = timestamp; Activity = xs } |> Some
        | _ -> None
        )
    |> Seq.choose id

let history =
    File.ReadAllLines(@"c:\projects\github\advent-of-code-2018\day4\input.dat")
    |> Seq.map parseRow
    |> Seq.choose id
    |> Seq.sortBy (fun a-> 
        match a with
        | Wakes t -> t
        | Sleeps t -> t
        | OnDuty (t,_) -> t
        )

let byDay = groupActivityByDay history

//byDay.Dump("Activity By Day")
    
let calculateNaptime days =
    days
    |> Seq.map (fun d ->
        d.Activity
        |> Seq.fold (fun (duration, lastTime) act -> 
            match act with
            | (Sleeps  t) -> (duration, t) 
            | (Wakes t) -> (duration + ((t - lastTime).TotalMinutes |> int) - 1, t) 
            | _ -> failwith "Invalid data"
        ) (0, d.Started) )
    |> Seq.sumBy fst


let activityById = byDay |> Seq.groupBy (fun r -> r.Id)

//activityById.Dump("Activity By ID")

let napTimes = 
    activityById
    |> Seq.map (fun r -> {Id = fst r; Duration = snd r |> calculateNaptime})
    |> Seq.sortByDescending (fun r -> r.Duration)

let lazyElf = napTimes |> Seq.head
printfn "Lazy Elf: ID: %d Duration: %d" lazyElf.Id lazyElf.Duration
    
let plotNaps history = 
    history
    |> Seq.map ( fun h -> 
        h.Activity 
        |> Seq.fold (fun (lastTime:DateTime,naps) action ->
        match action with
        | (Wakes t) -> lastTime, (lastTime,((t - lastTime).TotalMinutes |> int) - 1) :: naps
        | (Sleeps t) -> t,naps
        | _ -> failwith "Invalid case") (h.Started,[]) )
    |> Seq.collect snd
    |> Seq.collect (fun (start, duration) ->
        [for i in 0 .. duration -> 
            (start.AddMinutes(i|>float).ToString("hhmm"),1)])
    |> Seq.groupBy fst
    |> Seq.map ( fun (min, occurances) -> (min, occurances |> Seq.length))

let selectActivity id history = 
    history 
    |> Seq.filter (fun (id', _) -> id = id')
    |> Seq.map snd
    |> Seq.head

let naps = selectActivity lazyElf.Id activityById |> plotNaps

// naps.Dump()

let sleepiestMinuteEncoded = 
    naps
    |> Seq.sortByDescending (fun (_,count)-> count)
    |> Seq.head
    |> fst
    

let sleepiestMinute = sleepiestMinuteEncoded.Substring(2) |> int

printfn "Sleepiest Minute: %d" sleepiestMinute

printfn "Checksum:  %d" (lazyElf.Id * sleepiestMinute)