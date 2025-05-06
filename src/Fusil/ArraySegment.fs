module Fusil.ArraySegment

#if FABLE_COMPILER
type ArraySegment<'T>(arr: 'T array, offset: int) =
    member _.Item
        with get i = arr[i + offset]
        and set i v = arr[i + offset] <- v
#endif

let inline createArraySegment offset count arr =
    #if FABLE_COMPILER
    ArraySegment(arr, offset)
    #else
    System.ArraySegment(arr, offset, count)
    #endif