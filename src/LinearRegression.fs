module LinearRegression

// Returns (intercept * slope)
let Fit (x: float list) (y: float list) =
    let mx = x |> List.average
    let my = y |> List.average
    let difference = x |> List.map (fun e -> e - mx)

    let covariance =
        difference
        |> List.mapi (fun index diff -> diff * (y.[index] - my))
        |> List.sum

    let variance =
        difference
        |> List.map (fun diff -> diff * diff)
        |> List.sum

    let b = covariance / variance
    (my - b * mx, b)
