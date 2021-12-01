open System.IO

let input = 
    File.ReadAllLines @"input.txt"
    |> Array.map int

let solution1 = 
    input
    |> Array.pairwise 
    |> Array.filter (fun x -> fst x < snd x)
    |> Array.length


let solution2 = 
    input 
    |> Array.windowed 3
    |> Array.map (fun x -> x |> Array.sum)
    |> Array.pairwise 
    |> Array.filter (fun x -> fst x < snd x)
    |> Array.length