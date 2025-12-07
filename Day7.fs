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
    
let rec cascade (splits:int) (beams:int list) (rows: int list list)=
    match rows with
    | [] -> splits
    | head::tail ->
        let newSplits, beams' = splitBeams beams head
        cascade (splits+newSplits) beams' tail
        
        
let part1 fn () =
    let input = readInput fn |> Seq.map List.ofSeq |> List.ofSeq
    let startPos = input |> List.head |> findStart |> Seq.exactlyOne
    let rows = input |> List.tail |> List.map findSplitter
    cascade 0 [startPos] rows |> int64
    
let part2 fn () = 0L
