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