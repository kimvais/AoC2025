module AoC2025.Day1

open System.Text.RegularExpressions
open AoC2025.Prelude

type Direction = Left | Right

let parseDir =
    function 
    | "L" -> Left
    | "R" -> Right
    | _ -> failwith "Invalid direction"

let re = Regex(@"^(?<dir>[LR])(?<steps>\d+)$")

let turn acc (direction, steps) =
    match direction with
    | Left -> (acc - steps) % 100L
    | Right -> (acc + steps) % 100L
 
let inputToCommands fn =
    readInput fn
    |> Seq.map (fun line -> re.Match line)
    |> Seq.map (fun m -> (m.Groups.["dir"].Value |> parseDir), int64 m.Groups.["steps"].Value)

let countZeroes positions =
    positions |> (Seq.filter ((=) 0L) >> Seq.length >> int64)
    
let part1 fn () =
    inputToCommands fn
    |> Seq.scan turn 50L
    |> countZeroes
  
let folder (zeroes, startPos)  (direction, steps) =
    let op =
        match direction with
        | Left -> (-) 
        | Right -> (+)
    let newPositions = seq {1L..steps} |> Seq.map (op startPos >> fun a -> a % 100L)
    (zeroes + countZeroes newPositions, Seq.last newPositions)
    
let part2 fn () =
    inputToCommands fn
    |> Seq.fold folder (0L, 50L)
    |> fst
    

