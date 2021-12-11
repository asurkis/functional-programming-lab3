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
