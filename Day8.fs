module AoC2025.Day8

open System
open AoC2025.Prelude

let distance [vec1; vec2] =
    let coords = List.zip vec1 vec2
    coords |> List.map ((fun (a,b) -> pown (float a-float b) 2) >> float) |> List.sum |> sqrt

(*
let rec merge all rounds connected n =
    match n = rounds with
    | true -> connected
    | false -> 
        let unconnected = all - Set.ofList (connected |> List.concat)
        let pairs = Seq.allPairs all unconnected |> Seq.filter (fun (a,b) -> a <> b)
        let a,b = pairs |> Seq.minBy distance
        merge all rounds (connected @ [[a;b]]) (n + 1)
*)

let listTo3Tuple [x;y;z] = x,y,z

let rec comb n l = 
    match n, l with
    | 0, _ -> [[]]
    | _, [] -> []
    | k, (x::xs) -> List.map ((@) [x]) (comb (k-1) xs) @ comb k xs

let addConnection circuits (connection: (int*int*int) list)=
    let toJoin = Set.ofList connection
    let after = circuits |> List.groupBy (Set.intersect toJoin >> Set.isEmpty >> not)
    let circuits' = after |> Seq.filter fst |> Seq.exactlyOne |> snd |> Set.unionMany
    let oldCircuits = after |> List.filter (fst >> not) |> List.map snd |> List.concat
    [circuits'] @ oldCircuits
    
let rec connect circuits (connections: (int * int * int) list list) =
    match connections with
    | [head] ->
        addConnection circuits head
    | head::tail ->
        let circuits' = addConnection circuits head
        connect circuits' tail

let makeCircuitOf1 tup =
    Set.empty.Add(listTo3Tuple tup)
  
let part1 n fn () =
    let input = readInput fn |> Seq.map (split ',' >> Seq.map int >> List.ofSeq) |> List.ofSeq
    let connections = comb 2 input |> List.sortBy distance |> List.take n |> List.map (List.map listTo3Tuple)
    let circuits = connect (input |> List.map makeCircuitOf1) connections
    circuits |> List.map (Set.count >> int64) |> List.sortDescending |> List.take 3 |> List.reduce (*)

let part2 fn () =
    let input = readInput fn
    0L
