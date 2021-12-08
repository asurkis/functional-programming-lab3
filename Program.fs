open Lab3.IO
open Lab3.Math
open Microsoft.Extensions.Configuration
open System

[<EntryPoint>]
let main argv =
    let config =
        ConfigurationBuilder()
            .AddCommandLine(argv)
            .Build()

    let helpRequested =
        (Array.tryFind (fun s -> s = "--help") argv)
        <> None

    let start = config.GetValue<float Nullable> "start"
    let finish = config.GetValue<float Nullable> "finish"

    let sampleCount =
        config.GetValue<int Nullable> "sampleCount"

    let polynomDegree =
        config.GetValue<int Nullable> "polynomDegree"

    let allInputs =
        start.HasValue
        && finish.HasValue
        && sampleCount.HasValue
        && polynomDegree.HasValue

    if helpRequested || not allInputs then
        printfn "Usage:"
        printfn "\tlab3 --help"
        printfn "\tlab3 --start <number> --finish <number> --sampleCount <number> --polynomDegree <number>"
        0
    else
        if sampleCount.Value >= 2 && polynomDegree.Value >= 1 then
            let samples =
                samplesList start.Value finish.Value sampleCount.Value

            let model =
                inputLines
                |> inputPoints
                |> polynomial (polynomDegree.Value + 1)

            approximate samples model
            |> Seq.iter (fun (x, y) -> printfn "%f; %f" x y)
            0
        else
            if sampleCount.Value < 2 then
                printfn "At least two sample points are needed"
            if polynomDegree.Value < 1 then
                printfn "The polynom must be at least linear"
            1
