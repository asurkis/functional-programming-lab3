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

let splines =
    Seq.map2 (fun (x1, y1) (x2, y2) -> (x2, (fun x -> (y2 - y1) * (x - x1) / (x2 - x1))))
    <| inputPoints
    <| Seq.tail inputPoints

let rec calcSampleY splines x =
    (splines
     |> Seq.filter (fun (xr, _) -> x < xr)
     |> Seq.head
     |> snd)
    <| x

let sampleXs =
    seq { 0 .. approxPoints - 1 }
    |> Seq.map (fun i ->
        start
        + (finish - start) * float i
          / float (approxPoints - 1))

let sampleYs =
    sampleXs |> Seq.map (calcSampleY splines)

Seq.zip sampleXs sampleYs
|> Seq.map (sprintf "%A; ")
|> Seq.fold (+) ""
|> ignore
// |> printfn "[ %s]"

open System

let lines =
    Seq.initInfinite (fun _ -> Console.ReadLine())
lines |> Seq.map (fun x -> printfn "%A" x; x) |> Seq.contains null |> ignore
