[<AutoOpen>]
module Helpers

open System.Collections.Generic

// This kind of debouncer is part of the awesome F# Fabulous Library
// source: https://github.com/fsprojects/Fabulous/issues/161
// but modified for use in F# Fable
let debounce<'T> =
    let mutable memoizations = Dictionary<string, int> HashIdentity.Structural

    fun (timeout: int) (fn: 'T -> unit) value ->
        let key = fn.ToString()
        // Cancel previous debouncer
        match memoizations.TryGetValue key with
        | true, timeoutId -> Fable.Core.JS.clearTimeout timeoutId
        | _ -> ()

        // Create a new timeout and memoize it
        let timeoutId =
            Fable.Core.JS.setTimeout
                (fun () ->
                    memoizations.Remove key |> ignore
                    fn value
                )
                timeout
        memoizations[key] <- timeoutId