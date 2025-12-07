module AoC2025.Day7

open AoC2025
open AoC2025.Prelude

let findChar c s =
    s |> List.mapi (fun i c' -> 
        match c' with 
        | c'' when c'' = c -> Some i
        | _ -> None
        ) |> List.choose id
    
let findStart = findChar 'S'
let findSplitter = findChar '^'

let splitBeams beams row =
    let beams' =
            beams |> List.map (fun n ->
                if List.contains n row then
                    [n-1; n+1]
                else
                    [n]
                )
    let splits = beams' |> Seq.filter (fun s -> Seq.length s = 2) |> Seq.length
    let newBeams = beams' |> Seq.concat |> Set.ofSeq |> Set.toList
    splits, newBeams
 
let splitBeams2 beams row =
    let beams' =
        beams |> Map.toList |> List.map (fun (pos, count) ->
            if List.contains pos row then
                    let left = beams |> Map.tryFind (pos - 1) |> function
                        | Some n -> n + count
                        | None -> count
                    let right = beams |> Map.tryFind (pos + 1) |> function
                        | Some n -> n + count
                        | None -> count
                    [pos - 1, left; pos + 1, right]
                else [pos, count]
            ) |> List.concat |> List.groupBy fst |> List.map (fun (p, l) -> p, l |> List.sumBy snd)
        |> Map.ofList
    printfn "%A" beams'
    beams'
    
let rec cascade (splits:int) (beams:int list) (rows: int list list)=
    match rows with
    | [] -> splits
    | head::tail ->
        let newSplits, beams' = splitBeams beams head
        cascade (splits+newSplits) beams' tail
        
let rec cascade2 (beams: Map<int,int>) (rows: int list list) =
    match rows with
    | [] -> beams
    | head::tail ->
        let beams' = splitBeams2 beams head
        cascade2 beams' tail
        
let part1 fn () =
    let input = readInput fn |> Seq.map List.ofSeq |> List.ofSeq
    let startPos = input |> List.head |> findStart |> Seq.exactlyOne
    let rows = input |> List.tail |> List.map findSplitter
    cascade 0 [startPos] rows |> int64
    
let part2 fn () =
    let input = readInput fn |> Seq.map List.ofSeq |> List.ofSeq
    let startPos = input |> List.head |> findStart |> Seq.exactlyOne
    let rows = input |> List.tail |> List.map findSplitter
    let beams = Map [(startPos, 1)]
    cascade2 beams rows |> Map.toSeq |> Seq.sumBy snd 
