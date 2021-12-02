module Lab3.IO

open System

let inputLines =
    Seq.initInfinite (fun _ -> Console.ReadLine())

let inputPoints (inputLines: string seq) =
    inputLines
    |> Seq.takeWhile (fun s -> s <> null)
    |> Seq.map (fun s -> s.Split [| ';' |])
    |> Seq.map (Array.map Double.TryParse)
    |> Seq.choose (function
        | [| true, x; true, y |] -> Some(x, y)
        | _ ->
            printfn "Point should be written in \"<number>; <number>\" format"
            None)

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
