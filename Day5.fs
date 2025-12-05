module AoC2025.Day5

open System.Text.RegularExpressions
open AoC2025
open AoC2025.Prelude

let re = Regex(@"(?<lower>\d+)-(?<upper>\d+)")

let parseRange s =
    let m = re.Match(s)
    int64 m.Groups["lower"].Value, int64 m.Groups["upper"].Value

let checkFreshness fresh ingredient =
    fresh |> Array.exists (fun (l, u) -> l <= ingredient && u >= ingredient)

let part1 fn () =
    let [| freshness; ingredients |] =
        readInputDelimByEmptyLine fn |> Array.map splitByLinefeed

    let fresh = freshness |> Array.map parseRange
    let ingredients' = ingredients |> Array.map int64
    let isFresh = checkFreshness fresh
    ingredients' |> Array.filter isFresh |> Array.length |> int64

let mergeRanges ranges =
    let rec loop remaining accumulator =
        match remaining with
        | [] -> List.rev accumulator
        | head :: tail ->
            match accumulator with
            | [] -> loop tail [ head ]
            | currentMerged :: accTail ->
                let currStart, currEnd = currentMerged
                let nextStart, nextEnd = head
                if nextStart <= currEnd then
                    let newEnd = max currEnd nextEnd
                    let newMerged = (currStart, newEnd)
                    loop tail (newMerged :: accTail)
                else
                    loop tail (head :: accumulator)

    loop ranges []

let getRangeSize (l, u) =
    u - l + 1L
    
let part2 fn () =
    let freshness =
        readInputDelimByEmptyLine fn
        |> Array.map splitByLinefeed
        |> Array.head
        |> Array.map parseRange
        |> Array.sort
        |> List.ofArray
        
    mergeRanges freshness |> List.map getRangeSize |> List.sum 