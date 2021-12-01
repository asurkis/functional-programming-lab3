open System

let inputPoints =
    Seq.initInfinite (fun _ -> Console.ReadLine())
    |> Seq.takeWhile (fun s -> s <> null)
    |> Seq.map (fun s -> s.Split [| ';' |])
    |> Seq.map (Array.map Double.TryParse)
    |> Seq.choose (function
        | [| (true, x); (true, y) |] -> Some(x, y)
        | _ ->
            printfn "Point should be written in \"<number>; <number>\" format"
            None)

let linearApprox x1 y1 x2 y2 xs = y1 + (y2 - y1) * (xs - x1) / (x2 - x1)

let linear =
    inputPoints
    |> Seq.scan (fun (_, a) b -> (a, b)) ((0.0, 0.0), (0.0, 0.0))
    |> Seq.tail
    |> Seq.map (fun ((x1, y1), (x2, y2)) -> x2, linearApprox x1 y1 x2 y2)

let start = -2.0
let finish = 1.9
let approxPoints = 11

let sampleXs =
    seq { 0 .. approxPoints - 1 }
    |> Seq.map (
        float
        >> linearApprox 0.0 start (float (approxPoints - 1)) finish
    )

let approximator = linear

approximator
|> Seq.map (printfn "%A")
|> Seq.length
|> ignore
