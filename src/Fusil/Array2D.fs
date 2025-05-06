module internal Fusil.Array2D

open Fusil.Slab

#if FABLE_COMPILER
open Fable.Core
#endif

// Custom implementation of Array2D for Fable compatibility
#if FABLE_COMPILER
type Array2D<'T>(arr: 'T ArraySegment.ArraySegment, height, width) =
#else
type Array2D<'T>(arr: 'T System.ArraySegment, height, width) =
#endif
    let mutable arr = arr

    new(arr: 'T array, height, width) =
        Array2D(
            ArraySegment.createArraySegment 0 (height * width) arr,
            height,
            width
        )

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
    let allocFromSlab height width offset (slab: Slab) =
        let newOffset, arraySegment = slab |> Slab.alloc16 offset (height * width)
        newOffset, Array2D(arraySegment, height, width)

    let inline init height width init =
        let arr2d = Array2D(
            Array.zeroCreate<'T> (width * height),
            height,
            width
        )
        for i = 0 to height - 1 do
            for j = 0 to width - 1 do
                arr2d[i, j] <- init i j

        arr2d