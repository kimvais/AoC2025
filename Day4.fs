module AoC2025.Day4

open AoC2025.Prelude

let hasPaper =
    function
    | '@' -> true
    | _ -> false

let getNeighbourCoords maxX maxY x y =
    let inBounds x'' y'' =
        0 <= x'' && x'' <= maxX && 0 <= y'' && y'' <= maxY
    let deltas = [
          (-1, -1); (-1, 0); (-1, 1);
          ( 0, -1);          ( 0, 1);
          ( 1, -1); ( 1, 0); ( 1, 1);
    ]
    deltas |> Seq.choose (fun (dy, dx) ->
        let y' = y + dy
        let x' = x + dx
        if inBounds x' y' then Some (y', x') else None)

let part1 fn () =
    let grid =
        readInput fn |> Array.ofSeq |> Array.map (Array.ofSeq >> Array.map hasPaper)

    let getRoll (y, x) = grid.[y].[x]
    let maxY = Array.length grid - 1
    let maxX = Array.length grid.[0] - 1

    let rollCounts =
        seq {
            for y in [ 0 .. (Array.length grid) - 1 ] do
                for x in [ 0 .. (Array.length grid.[0]) - 1 ] do
                    if grid.[y].[x] then
                        yield getNeighbourCoords maxX maxY x y
        }

    rollCounts
    |> Seq.map (Seq.filter getRoll >> Seq.length)
    |> Seq.filter (fun n -> n < 4)
    |> Seq.length
    |> int64

let part2 fn () = 0L
