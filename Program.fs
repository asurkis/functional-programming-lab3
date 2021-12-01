open System

// let inputPoints =
//     Seq.initInfinite (fun _ -> Console.ReadLine())
//     |> Seq.takeWhile (fun s -> s <> null)
//     |> Seq.map (fun s -> s.Split [| ';' |])
//     |> Seq.map (Array.map Double.TryParse)
//     |> Seq.choose (function
//         | [| (true, x); (true, y) |] -> Some(x, y)
//         | _ ->
//             printfn "Point should be written in \"<number>; <number>\" format"
//             None)

let inputPoints =
    seq {
        (-2.0, 4.0)
        (-1.0, 1.0)
        (0.0, 0.0)
        (1.0, 1.0)
        (2.0, 4.0)
    }

let linearApprox x1 y1 x2 y2 xs = y1 + (y2 - y1) * (xs - x1) / (x2 - x1)

let linear =
    inputPoints
    |> Seq.scan (fun (_, a) b -> (a, b)) ((0.0, 0.0), (0.0, 0.0))
    |> Seq.tail
    |> Seq.map (fun ((x1, y1), (x2, y2)) -> x2, linearApprox x1 y1 x2 y2)

let approximator = linear

let start = -2.0
let finish = 2.0
let approxPoints = 21

let sampleXs =
    [ 0 .. approxPoints - 1 ]
    |> List.map (
        float
        >> linearApprox 0.0 start (float (approxPoints - 1)) finish
    )

(*
    Input points are a sequence,
    since the process should work as a pipe.
    They should also be only evaluated once,
    and Seq.cache is not free.
    Therefore iterating through them
    using a loop is preferrable.
*)
let samplesSplit =
    seq {
        let mutable samples = sampleXs

        (*
            Here we assign a function for interpolation
            to the sample coordinated
            based on the interval it falls into
        *)
        for xUp, f in approximator do
            let left =
                samples
                |> List.takeWhile (fun x -> x < xUp)
                |> List.map (fun x -> x, f)

            yield! left
            samples <- List.skip left.Length samples
    }

samplesSplit
|> Seq.map (fun (x, f) -> x, f x)
|> Seq.map (fun (x, y) -> printfn "%f %f" x y)
|> Seq.length
|> ignore
