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

let part1 fn () =
    readInput fn
    |> Seq.map (fun line -> re.Match line)
    |> Seq.map (fun m -> (m.Groups.["dir"].Value |> parseDir), int64 m.Groups.["steps"].Value)
    |> Seq.scan turn 50L
    |> Seq.filter ((=) 0L)
    |> Seq.length
    |> int64
    
let part2 fn () = 0L
    
    

