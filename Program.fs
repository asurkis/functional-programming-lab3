let start = -2.0
let finish = 1.9
let approxPoints = 11

let inputPoints =
    seq {
        (-2.0, 4.0)
        (-1.0, 1.0)
        (0.0, 0.0)
        (1.0, 1.0)
        (2.0, 4.0)
    }
    |> Seq.map (fun x ->
        printfn "Accessing %A" x
        x)

let linear =
    inputPoints
    |> Seq.scan (fun (_, a) b -> (a, b)) ((0.0, 0.0), (-infinity, 0.0))
    |> Seq.tail
    |> Seq.map (fun ((x1, y1), (x2, y2)) -> x2, fun x -> (y2 - y1) * (x - x1) / (x2 - x1))

let sampleXs =
    seq { 0 .. approxPoints - 1 }
    |> Seq.map (fun i ->
        start
        + (finish - start) * float i
          / float (approxPoints - 1))

linear
|> Seq.map (fun (x, f) ->
    sampleXs
    |> Seq.choose (fun sx -> if sx < x then Some(f sx) else None))
|> Seq.concat
|> Seq.map (fun x ->
    printfn "%A" x
    true)
|> Seq.contains false
|> ignore

open System

let lines =
    Seq.initInfinite (fun _ -> Console.ReadLine())

lines
|> Seq.map (fun x ->
    printfn "%A" x
    x)
|> Seq.contains null
|> ignore
