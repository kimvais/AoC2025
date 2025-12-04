module AoC2025.Day4

open AoC2025
open AoC2025.Prelude

let hasPaper = function
    | '@' -> true
    | _ -> false
   
let findNeighbours (arr: 'a array array) x y=
    seq {
    if y > 0 then
        if x > 0 then yield arr.[y-1][x-1]
        yield arr.[y-1][x]
        if x < Array.length arr - 1 then yield arr.[y-1][x+1]
    if x > 0 then yield arr.[y][x-1]
    if x < Array.length arr - 1 then yield arr.[y][x+1]
    if y < Array.length arr - 1 then 
        if x > 0 then yield arr.[y+1][x-1]
        yield arr.[y+1][x]
        if x < Array.length arr - 1 then yield arr.[y+1][x+1]
    }
    
let part1 fn () =
    let grid = readInput fn |> Array.ofSeq |> Array.map (Array.ofSeq >> Array.map hasPaper)
    let rollCounts =
        seq {
            for y in [0..(Array.length grid) - 1] do
                for x in [0..(Array.length grid.[0]) - 1] do
                    if grid.[y].[x] then 
                        yield findNeighbours grid x y
    }
    rollCounts |> Seq.map (Seq.filter id >> Seq.length) |> Seq.filter (fun n -> n < 4) |> Seq.length |> int64
    
let part2 fn () =
    0L