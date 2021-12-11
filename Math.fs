module Lab3.Math

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

let polynomial n inputPoints =
    inputPoints
    |> Seq.windowed n
    |> Seq.map (fun arr -> Array.head arr, Array.last arr, arr)
    |> Seq.map (fun ((x1, _), (x2, _), arr) -> x1, x2, polynomialApprox arr)

let samplesList start finish count =
    [ 0 .. count - 1 ]
    |> List.map float
    |> polynomialApprox [| 0.0, start
                           float (count - 1), finish |]

(*
    Input points are a sequence, since the process should work as a pipe.
    They should also be only evaluated once, and Seq.cache is not free.
    Therefore iterating through them using a loop is preferrable.

    This is not exactly the same as a directed join,
    as here we don't have key equivalence.
*)
let gatherFunctions samples model =
    seq {
        let mutable samples = samples

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

let approximate samples model =
    gatherFunctions samples model
    |> Seq.map (fun (f, xs) -> List.zip xs (f xs))
    |> Seq.concat
