module App

open Fable.Core.JsInterop
open Browser
open Fusil

let textInput = document.querySelector "#city-text-input" :?> Types.HTMLInputElement
let resultsDiv = document.querySelector "#results"

let slab = Slab.createDefault()

textInput.oninput <- fun evt ->
    let newText = evt.target?value

    // Compute the results
    let sortedCities =
        Cities.fr
        |> Array.choose (fun city ->
            match Fusil.fuzzyMatch false true true slab newText city with
            | Some x -> Some (city, x)
            | None -> None
        )
        |> Array.sortByDescending (fun (city, res) -> res.Score, -city.Length)
        |> Array.map (fun (city, res) -> city, res.MatchingPositions)

    // Clear previous results
    resultsDiv.innerHTML <- ""

    // Display results
    sortedCities
    |> Array.take (min sortedCities.Length 30)
    |> Array.iter (fun (city, pos) ->
        let span = document.createElement "span"

        for i in 0..city.Length-1 do
            match pos.Contains i with
            | true ->
                let s = document.createElement "strong"
                s.textContent <- unbox city[i]
                s :> Types.Node
            | false -> document.createTextNode <| unbox city[i] :> Types.Node
            |> span.appendChild
            |> ignore

        resultsDiv.appendChild span |> ignore
    )