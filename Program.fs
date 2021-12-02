open Lab3.IO
open Lab3.Math

[<EntryPoint>]
let main argv =
    match parseArgs argv with
    | Ok (parameters) ->
        let samples =
            samplesList parameters.Start parameters.Finish parameters.SampleCount

        let model =
            inputLines
            |> inputPoints
            |> polynomial (parameters.PolynomDegree + 1)

        approximate samples model
        |> Seq.iter (fun (x, y) -> printfn "%f; %f" x y)

        0
    | Error (s) ->
        printfn "%s" s
        1
