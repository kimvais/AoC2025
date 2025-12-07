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

    let result = 
        match day with
        | "1" -> Day1.part1 "1" ()
        | "1b" -> Day1.part2 "1" ()
        | "2" -> Day2.part1 "2" ()
        | "2b" -> Day2.part2 "2" ()
        | "3" -> Day3.part1 "3" ()
        | "3b" -> Day3.part2 "3" ()
        | "4" -> Day4.part1 "4" ()
        | "4b" -> Day4.part2 "4" ()
        | "5" -> Day5.part1 "5" ()
        | "5b" -> Day5.part2 "5" ()
        | "6" -> Day6.part1 "6" ()
        | "6b" -> Day6.part2 "6" ()
        | "7" -> Day7.part1 "7" ()
        | "7b" -> Day7.part2 "7" ()
        | "test" -> Day7.part2 "test7" ()
        | s -> failwith $"Invalid choice {s}"

    stopWatch.Stop()
    printfn "Ran for %0.3f seconds\n" stopWatch.Elapsed.TotalSeconds
    printfn "%d\n" result
    0
