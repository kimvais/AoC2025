module AoC2025.Tests

open AoC2025


open FsUnit.Xunit
open Xunit


[<Fact>]
let ``day 1, part 1`` () =
    Day1.part1 "test1" () |> should equal 3L
    Day1.part1 "1" () |> should equal 1132L

[<Fact>]
let ``day1, part2`` () =
    Day1.part2 "test1" () |> should equal 6L
    Day1.part2 "1" () |> should equal 6623L

[<Fact>]
let ``day2, part1`` () =
    Day2.part1 "test2" () |> should equal 1227775554L
    Day2.part1 "2" () |> should equal 21139440284L

[<Fact>]
let ``day2 , part2`` () =
    Day2.part2 "test2" () |> should equal 4174379265L
    Day2.part2 "2" () |> should equal 38731915928L

[<Fact>]
let ``day3, part1`` () =
    Day3.part1 "test3" () |> should equal 357L
    Day3.part1 "3" () |> should equal 17435L

[<Fact>]
let ``day3, part2`` () =
    Day3.part2 "test3" () |> should equal 3121910778619L
    Day3.part2 "3" () |> should equal 172886048065379L

[<Fact>]
let ``day4, part1`` () =
    Day4.part1 "test4" () |> should equal 13L
    Day4.part1 "4" () |> should equal 1451L

[<Fact>]
let ``day4, part2`` () =
    Day4.part2 "test4" () |> should equal -1L
    Day4.part2 "4" () |> should equal -1L

[<Fact>]
let ``day5, part1`` () =
    Day5.part1 "test5" () |> should equal 3L
    Day5.part1 "5" () |> should equal 756L

[<Fact>]
let ``day5, part2`` () =
    Day5.part2 "test5" () |> should equal 14L
    Day5.part2 "5" () |> should equal 355555479253787L
