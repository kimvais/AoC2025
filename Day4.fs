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

let removeRolls (grid: bool [][]) : bool [][] =
    let maxY = Array.length grid - 1
    let maxX = Array.length grid.[0] - 1

    let getRoll (y,x) = grid.[y].[x]

    grid
    |> Array.mapi (fun y row ->
        row
        |> Array.mapi (fun x hasRoll ->
            if not hasRoll then false
            else
                let neighbourCount =
                    getNeighbourCoords maxX maxY x y
                    |> Seq.filter getRoll
                    |> Seq.length
                neighbourCount >= 4))

let countRolls grid =
    grid |> Array.map Seq.ofArray |> Seq.ofArray |> Seq.concat |> Seq.filter id  |> Seq.length |> int64

let rec removeAll removed grid =
    let rolls = countRolls grid
    let grid' = removeRolls grid
    let newRolls = countRolls grid'
    match rolls - newRolls with
    | 0L -> removed
    | n -> removeAll (removed + n ) grid'
    
let part1 fn () =
    let grid =
        readInput fn |> Array.ofSeq |> Array.map (Array.ofSeq >> Array.map hasPaper)

    let rolls = countRolls grid
    let grid' = removeRolls grid
    let newRolls = countRolls grid'
    rolls - newRolls

let part2 fn () =
    let grid =
        readInput fn |> Array.ofSeq |> Array.map (Array.ofSeq >> Array.map hasPaper)
    removeAll 0L grid
    
