open System.IO
open System.Text.RegularExpressions

type SubmarineCommands =
    | Forward of int
    | Up of int
    | Down of int
    | Imposibru

let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern)

    if m.Success then
        Some(List.tail [ for g in m.Groups -> g.Value ])
    else
        None

let parseCommand (cmd: string) =
    match cmd with
    | Regex "(forward|up|down) ([1-9])" [ command; value ] ->
        match command with
        | "forward" -> Forward(int value)
        | "up" -> Up(int value)
        | "down" -> Down(int value)
        | _ -> Imposibru
    | _ -> failwith "wrong command format"

let commands =
    File.ReadAllLines @"C:\git\playground\aoc2021\day2\input.txt"
    |> Array.map parseCommand

type SubmarinePosition = { Horizontal: int; Depth: int }

let executeCommand position submarineCmd =
    match submarineCmd with
    | Forward value ->
        { position with
              Horizontal = position.Horizontal + value }
    | Down value ->
        { position with
              Depth = position.Depth + value }
    | Up value ->
        { position with
              Depth = position.Depth - value }
    | Imposibru -> failwith "Ohh shit!"

let initialPosition = { Horizontal = 0; Depth = 0 }

let calculateSolution1 position = position.Horizontal * position.Depth

let solution1 =
    commands
    |> Array.fold executeCommand initialPosition
    |> calculateSolution1

type AimPosition =
    { Horizontal: int
      Depth: int
      Aim: int }

let initialAimPosition =  { Horizontal = 0; Depth = 0; Aim = 0 }

let executeAimCommand (aimPosition: AimPosition) submarineCmd = 
    match submarineCmd with
    | Forward value ->
        { aimPosition with
              Horizontal = aimPosition.Horizontal + value 
              Depth = aimPosition.Aim * value + aimPosition.Depth }
    | Down value ->
        { aimPosition with
              Aim = aimPosition.Aim + value }
    | Up value ->
        { aimPosition with
              Aim = aimPosition.Aim - value }
    | Imposibru -> failwith "Ohh shit!"

let calculateSolution2 (pos: AimPosition) = pos.Horizontal * pos.Depth

let solution2 = 
    commands
    |> Array.fold executeAimCommand initialAimPosition
    |> calculateSolution2