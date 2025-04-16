module App

open Sutil
open Sutil.CoreElements

let view() =
    let results = Store.make List.empty

    let textChanged newText =
        let sortedCities =
            Cities.fr
            |> Array.choose (fun city ->
                match Fusil.fuzzyMatch newText city with
                | Some x -> Some (city, x)
                | None -> None
            )
            |> Array.sortByDescending (fun (city, (score, _)) -> score, -city.Length)
            |> Array.map (fun (city, (_score, pos)) -> city, pos)

        sortedCities
        |> Array.take (min sortedCities.Length 30)
        |> Array.toList
        |> Store.set results


    fragment [
        disposeOnUnmount [ results ]

        Html.h1 "Search a city"

        Html.input [
            Attr.typeText
            Attr.placeholder "Nantes"
            Ev.onTextInput textChanged
        ]

        Html.div [
            Attr.className "results"

            Bind.each (results, fun (city, pos) ->
                Html.span [
                    for i in 0..city.Length-1 do
                        match pos[i] with
                        | true -> Html.strong (text <| string city[i])
                        | false -> text <| string city[i]
                ]
            )
        ]
    ]

view() |> Program.mount