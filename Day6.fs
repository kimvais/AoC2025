module AoC2025.Day6

open AoC2025.Prelude

let parseOp = function
    | "*" -> (*)
    | "+" -> (+)
    | op -> failwith $"Invalid operation '{op}'"

let calculate s =
    let op = Seq.head s |> parseOp
    let numbers = Seq.tail s |> Seq.map int64
    numbers |> Seq.reduce op
    
let part1 fn () =
    let input = readInput fn |> Seq.rev |> Seq.map ((splitS "\s+") >> Seq.filter ((<>) ""))
    let instructions = input |> Seq.transpose 
    instructions |> Seq.map calculate |> Seq.sum
    
    
let part2 fn () = 0L