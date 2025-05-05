module App

open Fable.Core.JsInterop
open Browser

let textInput = document.querySelector "#city-text-input" :?> Types.HTMLInputElement
let resultsDiv = document.querySelector "#results"

let slab = Shared.Slab.Slab.createDefault()

textInput.oninput <- fun evt ->
    let newText = evt.target?value

    // Compute the results
    let sortedCities =
        Cities.fr
        |> Array.choose (fun city ->
            match Fusil.fuzzyMatch slab false true newText city with
            | Some x -> Some (city, x)
            | None -> None
        )
        |> Array.sortByDescending (fun (city, (score, _)) -> score, -city.Length)
        |> Array.map (fun (city, (_score, pos)) -> city, pos)

    // Clear previous results
    resultsDiv.innerHTML <- ""

    // Display results
    sortedCities
    |> Array.take (min sortedCities.Length 30)
    |> Array.iter (fun (city, pos) ->
        let span = document.createElement "span"

        for i in 0..city.Length-1 do
            match pos[i] with
            | true ->
                let s = document.createElement "strong"
                s.textContent <- unbox city[i]
                s :> Types.Node
            | false -> document.createTextNode <| unbox city[i] :> Types.Node
            |> span.appendChild
            |> ignore

        resultsDiv.appendChild span |> ignore
    )