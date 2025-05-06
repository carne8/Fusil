module Shared.Slab

type Slab =
    { i16: int16 array
      i32: int32 array }

[<RequireQualifiedAccess>]
module Slab =
    // Same as fzf slabs
    let [<Literal>] SLAB_16_SIZE = 100 * 1024 // 200 Ko
    let [<Literal>] SLAB_32_SIZE = 2048 // 8 Ko

    let createDefault () =
        { i16 = Array.zeroCreate SLAB_16_SIZE
          i32 = Array.zeroCreate SLAB_32_SIZE }

    let alloc16 offset size slab =
        offset + size, slab.i16 |> Shared.ArraySegment.createArraySegment offset size

    let alloc32 offset size slab =
        offset + size, slab.i32 |> Shared.ArraySegment.createArraySegment offset size