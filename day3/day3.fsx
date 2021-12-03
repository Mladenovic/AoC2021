open System.IO

let inline charToInt c = int c - int '0'

let input =
    File.ReadAllLines @"C:\git\playground\aoc2021\day3\input.txt"
    |> Array.map Seq.toArray
    |> Array.map (Array.map charToInt)

let sumTwoMetrics (x: int []) (y: int []) =
    x |> Array.mapi (fun i _ -> x.[i] + y.[i])

let transformToGama y x = if (y - x) < y / 2 then 1 else 0

let arr =
    input
    |> Array.reduce sumTwoMetrics
    |> Array.map (transformToGama input.Length)

let toEpsilonDigit x = if x = 1 then 0 else 1

let gama =
    arr
    |> Array.rev
    |> Array.mapi (fun i x -> (pown 2 i) * x)
    |> Array.sum

let epsilon =
    arr
    |> Array.map toEpsilonDigit
    |> Array.rev
    |> Array.mapi (fun i x -> (pown 2 i) * x)
    |> Array.sum

let solution = epsilon * gama

let filterPosition (input: int [] []) (pos: int) predicate =
    let bitSum =
        input |> Array.map (fun x -> x.[pos]) |> Array.sum

    let filter y x =
        if (predicate (y - x) (y / 2)) then
            1
        else
            0

    let filterBit = filter input.Length bitSum

    input
    |> Array.filter (fun x -> x.[pos] = filterBit)

let rec calculateStuff (input: int [] []) (pos: int) predicate =
    if input.Length = 1 then
        input.[0]
        |> Array.rev
        |> Array.mapi (fun i x -> (pown 2 i) * x)
        |> Array.sum
    else
        let newInput = filterPosition input pos predicate
        calculateStuff newInput (pos + 1) predicate

let calculateOxygenStuff =
    calculateStuff input 0 (fun x y -> x <= y)

let calculateCo2Stuff =
    calculateStuff input 0 (fun x y -> x > y)

let soulution2 = calculateOxygenStuff * calculateCo2Stuff
