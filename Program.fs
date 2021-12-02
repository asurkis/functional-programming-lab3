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

let polynomialApprox knownPoints samples =
    let polynome i x =
        knownPoints
        |> Array.mapi (fun j (xj, _) -> if i = j then 1.0 else x - xj)
        |> Array.reduce (*)

    let polynomeCoefficients =
        knownPoints
        |> Array.mapi (fun i (xi, yi) -> yi / polynome i xi)

    samples
    |> List.map (fun x ->
        seq { 0 .. knownPoints.Length - 1 }
        |> Seq.sumBy (fun i -> polynomeCoefficients.[i] * polynome i x))

let polynomial n =
    inputPoints
    |> Seq.windowed n
    |> Seq.map (fun arr -> Array.head arr, Array.last arr, arr)
    |> Seq.map (fun ((x1, _), (x2, _), arr) -> x1, x2, polynomialApprox arr)


let start = -2.0
let finish = 2.0
let approxPoints = 21

let sampleXs =
    [ 0 .. approxPoints - 1 ]
    |> List.map float
    |> polynomialApprox [| (0.0, start)
                           ((float (approxPoints - 1)), finish) |]

let model = polynomial 3

(*
    Input points are a sequence, since the process should work as a pipe.
    They should also be only evaluated once, and Seq.cache is not free.
    Therefore iterating through them using a loop is preferrable.

    This is not exactly the same as a directed join,
    as here we don't have key equivalence.
*)
let result =
    seq {
        let mutable samples = sampleXs

        (*
            Here we assign a function for interpolation
            to the sample coordinate based on the interval it falls into
        *)
        for xDown, xUp, f in model do
            samples <- List.skipWhile (fun x -> x < xDown) samples

            let left =
                samples |> List.takeWhile (fun x -> x < xUp)

            yield f, left
            samples <- List.skip left.Length samples
    }

result
|> Seq.map (fun (f, xs) -> List.zip xs (f xs))
|> Seq.concat
|> Seq.iter (fun (x, y) -> printfn "%f; %f" x y)
