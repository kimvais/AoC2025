module AoC2025.Day5

open System.Text.RegularExpressions
open AoC2025
open AoC2025.Prelude

let re = Regex(@"(?<lower>\d+)-(?<upper>\d+)")
let parseRange s =
    let m = re.Match(s)
    int64 m.Groups["lower"].Value, int64 m.Groups["upper"].Value
let checkFreshness fresh ingredient =
    fresh |> Array.exists (fun (l,u) -> l <= ingredient && u >= ingredient)

let part1 fn () =
    let [|freshness; ingredients|] = readInputDelimByEmptyLine fn |> Array.map splitByLinefeed
    let fresh = freshness |> Array.map parseRange 
    let ingredients' = ingredients |> Array.map int64
    let isFresh = checkFreshness fresh
    ingredients' |> Array.filter isFresh |> Array.length |> int64
    
let part2 fn () = 0L