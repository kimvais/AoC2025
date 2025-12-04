module AoC2025.Tests

open AoC2025


open FsUnit.Xunit
open Xunit


[<Fact>]
let ``day 1, part 1`` () =
    Day1.part1 "test1" () |> should equal 3L
    Day1.part1 "1" () |> should equal 0L

[<Fact>]
let ``day1, part2`` () =
    Day1.part2 "test1" () |> should equal 6L
    Day1.part2 "1" () |> should equal 0L

[<Fact>]
let ``day2, part1`` () =
    Day2.part1 "test2" () |> should equal 1227775554L
    Day2.part1 "2" () |> should equal 21139440284L

[<Fact>]
let ``day2 , part2`` () =
    Day2.part2 "test2" () |> should equal 4174379265L
