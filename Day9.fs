module AoC2025.Day9

open AoC2025.Prelude

let area ((x1, y1), (x2, y2)) =
    (abs x1-x2+1L) * (abs y1-y2+1L)
    
let part1 fn () =
    let input = readInput fn |> Seq.map (split ',' >> Seq.map int64 >> List.ofSeq >> fun [x;y] -> x,y) |> Seq.cache
    Seq.allPairs input input |> Seq.maxBy area |> area
    
let part2 fn () =
    let input = readInput fn
    0L

