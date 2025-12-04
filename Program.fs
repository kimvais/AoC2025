module AoC2025.Main

open System.Runtime.InteropServices
open AoC2025.Prelude
open AoC2025

module Kernel =
    [<DllImport("kernel32.dll", CharSet = CharSet.Auto, SetLastError = true)>]
    extern bool SetConsoleOutputCP(uint32 wCodePageID)

[<EntryPoint>]
let main argv =
    Kernel.SetConsoleOutputCP 65001u |> ignore
    let day = argv |> getProblem
    let stopWatch = System.Diagnostics.Stopwatch.StartNew()

    match day with
    | "1" -> Day1.part1 "1" ()
    | "1b" -> Day1.part2 "1" ()
    | "2" -> Day2.part1 "2" ()
    | "2b" -> Day2.part2 "2" ()
    | "3" -> Day3.part1 "3" ()
    | "3b" -> Day3.part2 "3" ()
    | "4" -> Day4.part1 "4" ()
    | "4b" -> Day4.part2 "4" ()
    | "test" -> Day4.part1 "test4" ()
    |> printfn "%d"
    |> ignore

    stopWatch.Stop()
    printfn "Ran for %0.3f seconds" stopWatch.Elapsed.TotalSeconds
    0
