module internal Shared.Array2D

// Custom implementation of Array2D for better performances than default one
// and for Fable compatibility
type Array2D<'T>(arr: 'T array, height, width) =
    member _.Width = width
    member _.Height = height

    member inline _.Get i j = arr[i * width + j]
    member inline _.Set i j v = arr[i * width + j] <- v
    member this.Item
        with get(i, j) = this.Get i j
        and set(i, j) v = this.Set i j v

module Array2D =
    let inline create height width defaultValue =
        let arr2d = Array2D(Array.zeroCreate<'T> (width * height), height, width)
        for i = 0 to height - 1 do
            for j = 0 to width - 1 do
                arr2d[i, j] <- defaultValue

        arr2d

    let inline init height width init =
        let arr2d = Array2D(Array.zeroCreate<'T> (width * height), height, width)
        for i = 0 to height - 1 do
            for j = 0 to width - 1 do
                arr2d[i, j] <- init i j

        arr2d