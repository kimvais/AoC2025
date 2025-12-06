module AoC2025.Day6

open AoC2025.Prelude

let parseOp =
    function
    | "*" -> (*)
    | "+" -> (+)
    | op -> failwith $"Invalid operation '{op}'"

let calculate s =
    let op = Seq.head s |> parseOp
    let numbers = Seq.tail s |> Seq.map int64
    numbers |> Seq.reduce op

let findFWFSpans (rows: string seq) =
    [ -1 ] // Because of begin+1..end-1
    @ (rows
       |> Seq.transpose
       |> Seq.mapi (fun i column ->
           match column |> Seq.forall ((=) ' ') with
           | true -> Some i
           | false -> None)
       |> Seq.choose id
       |> List.ofSeq)
    @ [ String.length (Seq.head rows) ]
    |> List.pairwise
    |> List.map (fun (a, b) -> a + 1, b - 1)

let rec splitByColumns (columnSpans: (int * int) list) acc (s: string) =
    match columnSpans with
    | [ start, end' ] ->

        let value = s.[start..end']
        acc @ [ value ]
    | head :: tail ->
        let start, end' = head
        let value = s.[start..end']
        let acc' = acc @ [ value ]
        splitByColumns tail acc' s

let part1 fn () =
    let input =
        readInput fn |> Seq.rev |> Seq.map ((splitS "\s+") >> Seq.filter ((<>) ""))

    let instructions = input |> Seq.transpose
    instructions |> Seq.map calculate |> Seq.sum

let part2 fn () =
    let input = readInput fn
    let delims = input |> findFWFSpans

    let operations =
        input
        |> Seq.map (splitByColumns delims [])
        |> Seq.rev
        |> Seq.transpose
        |> Seq.rev

    operations
    |> Seq.map (fun col ->
        let opStr = (Seq.head col).Trim()
        let op = parseOp opStr

        let numbers =
            col
            |> Seq.tail
            |> Seq.transpose
            |> Seq.map (Seq.rev >> System.String.Concat >> int64)
            |> Seq.rev

        numbers |> Seq.reduce op)
    |> Seq.sum
