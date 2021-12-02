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

let samplesList start finish count =
    [ 0 .. count - 1 ]
    |> List.map float
    |> polynomialApprox [| (0.0, start)
                           (float (count - 1), finish) |]

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

type Parameters =
    { Start: float
      Finish: float
      SampleCount: int
      PolynomDegree: int }

type CliOptionsIntermediate =
    { Start: float option
      Finish: float option
      SampleCount: int option
      PolynomDegree: int option }

let rec parseArgsHelper (intermediate: CliOptionsIntermediate) argv =
    match argv with
    | [] ->
        match intermediate with
        | { Start = None } -> Error("Start point of sampling is unknown")
        | { Finish = None } -> Error("Finish point of sampling is unknown")
        | { SampleCount = None } -> Error("Sample count is unknown")
        | { PolynomDegree = None } -> Error("Polynom degree is unknown")
        | { SampleCount = Some (sampleCount) } when sampleCount < 2 -> Error("Need at least 2 samples")
        | { PolynomDegree = Some (polynomDegree) } when polynomDegree < 1 ->
            Error("Only polynomial approximation with degree >= 1 is supported")
        | { Start = Some (start)
            Finish = Some (finish)
            SampleCount = Some (sampleCount)
            PolynomDegree = Some (polynomDegree) } ->
            Ok(
                { Parameters.Start = start
                  Parameters.Finish = finish
                  Parameters.SampleCount = sampleCount
                  Parameters.PolynomDegree = polynomDegree }
            )
    | "-f" :: s :: args ->
        match Double.TryParse s with
        | true, num -> parseArgsHelper { intermediate with Start = Some(num) } args
        | _ -> Error(sprintf "%s is not a number" s)
    | "-t" :: s :: args ->
        match Double.TryParse s with
        | true, num -> parseArgsHelper { intermediate with Finish = Some(num) } args
        | _ -> Error(sprintf "%s is not a number" s)
    | "-n" :: s :: args ->
        match Int32.TryParse s with
        | true, num -> parseArgsHelper { intermediate with SampleCount = Some(num) } args
        | _ -> Error(sprintf "%s is not an integer" s)
    | "-d" :: s :: args ->
        match Int32.TryParse s with
        | true, num -> parseArgsHelper { intermediate with PolynomDegree = Some(num) } args
        | _ -> Error(sprintf "%s is not an integer" s)
    | s :: _ -> Error(sprintf "Unexpected %s" s)

let parseArgs argv =
    parseArgsHelper
        { Start = None
          Finish = None
          SampleCount = None
          PolynomDegree = None }
        (List.ofArray argv)

[<EntryPoint>]
let main argv =
    match parseArgs argv with
    | Ok (parameters) ->
        let samples =
            samplesList parameters.Start parameters.Finish parameters.SampleCount

        let model =
            polynomial (parameters.PolynomDegree + 1)

        approximate samples model
        |> Seq.iter (fun (x, y) -> printfn "%f; %f" x y)

        0
    | Error (s) ->
        printfn "%s" s
        1
