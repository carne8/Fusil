module internal Shared.Array2D

#if FABLE_COMPILER
open Fable.Core
#endif

// Custom implementation of Array2D for Fable compatibility
type Array2D<'T>(arr: 'T array, height, width) =
    member _.Width = width
    member _.Height = height

    #if FABLE_COMPILER
    member inline _.Get i j : 'T = JsInterop.emitJsExpr (arr, i, width, j) "$0[$1 * $2 + $3]"
    member inline _.Set i j (v: 'T) = JsInterop.emitJsExpr (arr, i, width, j, v) "$0[$1 * $2 + $3] = $4"
    #else
    member inline _.Get i j = arr[i * width + j]
    member inline _.Set i j v = arr[i * width + j] <- v
    #endif
    member this.Item
        with get(i, j) = this.Get i j
        and set(i, j) v = this.Set i j v

#if FABLE_COMPILER
module Array =
    // Create an empty array instead of create one and filling it with nulls
    let inline zeroCreate<'T> count : 'T array = JsInterop.emitJsExpr count "new Array($0)"
#endif

module Array2D =
    let inline zeroCreate height width =
        Array2D(Array.zeroCreate<'T> (width * height), height, width)

    let inline create height width defaultValue =
        let arr2d = zeroCreate height width
        for i = 0 to height - 1 do
            for j = 0 to width - 1 do
                arr2d[i, j] <- defaultValue

        arr2d

    let inline init height width init =
        let arr2d = zeroCreate height width
        for i = 0 to height - 1 do
            for j = 0 to width - 1 do
                arr2d[i, j] <- init i j

        arr2d