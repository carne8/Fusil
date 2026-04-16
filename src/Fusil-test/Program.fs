open BenchmarkDotNet.Running
open Fusil
open Fusil.TextNormalization

open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Jobs
open Fusil.Fusil

[<SimpleJob(RuntimeMoniker.Net90)>]
[<MemoryDiagnoser>]
type Tests() =
    [<Params("Jaq", "y", "uuuuuuuuuuuuuuuuuu")>]
    member val query = "" with get, set
    member val slab = Slab.createDefault()

    [<Benchmark>]
    member this.Fusil() =
        let query = this.query |> String.normalize
        Cities.fr
        |> Array.choose (fuzzyMatch false true true this.slab query)
        |> Array.sortBy _.Score

//     // member val arrTuple = Cities.fr |> Array.map (fun s -> s.Length, s)
//     // member val arrStructTuple = Cities.fr |> Array.map (fun s -> struct (s.Length, s))
//     // [<Benchmark>]
//     // member this.SortInPlace() =
//     //     this.arr |> Array.sortInPlace
//     // [<Benchmark>]
//     // member this.Sort() =
//     //     this.arr |> Array.sort

//     // [<Benchmark>]
//     // member this.ParallelSort() =
//     //     this.arr |> Array.Parallel.sort
//     // [<Benchmark>]
//     // member this.ParallelSortInPlace() =
//     //     this.arr |> Array.Parallel.sortInPlace

//     // [<Benchmark>]
//     // member this.Tuple() =
//     //     Cities.fr
//     //     |> Array.map (fun s -> Some (s, s.Length))
//     //     |> Array.map (fun (Some (s, l)) -> Some (l, s))

//     // [<Benchmark>]
//     // member this.StructTuple() =
//     //     Cities.fr
//     //     |> Array.map (fun s -> Some (struct (s, s.Length)))
//     //     |> Array.map (fun (Some struct (s, l)) -> Some (struct (l, s)))

//     member val list = ResizeArray Cities.fr

//     [<Benchmark>]
//     member this.Simple() =
//         this.list.Sort(fun s1 s2 -> compare (s1.Length, s1) (s2.Length, s2))

//     [<Benchmark>]
//     member this.Comparison() =
//         this.list.Sort(System.Comparison(fun (s1: string) s2 -> compare (s1.Length, s1) (s2.Length, s2)))


BenchmarkRunner.Run<Tests>() |> ignore






    // [<Benchmark>]
    // member this.FuzzySharp() =
    //     FuzzySharp.Process.ExtractSorted(this.query, Cities.fr).GetEnumerator()





// let printResult s (resOpt: Fusil.FuzzyResult option) =
//     match resOpt with
//     | None -> ()
//     | Some (fuzzyResult: Fusil.FuzzyResult) ->
//         printfn "%s: %i" s fuzzyResult.Score
//         for i = 0 to s.Length-1 do
//             match fuzzyResult.MatchingPositions[i] with
//             | true -> printf "^"
//             | false -> printf " "
//         printfn ""
//
// let slab = Slab.createDefault()
//
// Fusil.fuzzyMatch false true true slab ("na" |> String.normalize) "L'Abergement-Clémenciat"
// |> printResult "L'Abergement-Clémenciat"

// fuzzyMatch false true "Visual Studio Code" ("vscode".ToCharArray()) true slab
// |> function
// | None -> printfn "None"
// | Some (a, b, c, arr) ->
//     printfn "%A" (a, b, c)
//     printfn "%A" arr
// fuzzyMatch false true "Tata" ("ta".ToCharArray()) true slab
// |> printfn "%A"
// fuzzyMatch slab false true "vscode" "Visual Studio Code" |> printResult "Visual Studio Code"
// fuzzyMatch slab false true "a" "abcd" |> printResult "abcd"
// fuzzyMatch slab false true "a" "tota" |> printResult "tota"
// fuzzyMatch slab false true "at" "tato" |> printResult "tato"
// fuzzyMatch slab false true "" "tato" |> printResult "tato"

// printfn "Waiting you"
// System.Console.ReadLine() |> ignore

// let slab = Shared.Slab.Slab.createDefault()
// let queries = [| "nantes"; "jaq"; "y"; "uuuuuuuuuuuuuuuuuu" |]
// let cities =
//     [| "Arboys en Bugey"
//        "Juillac-le-Coq"
//        "Nantes" |]

// queries
// |> Array.iter (fun query ->
//     printfn "\n\n--- %A" query
//     for city in cities do
//         printfn "\n%s" city
//         fuzzyMatch slab false true query city |> ignore
// )



// queries
// |> Array.iter (fun query ->
//     for city in Cities.usa do
//         fuzzyMatch query city |> ignore
// )

// System.Console.Read() |> ignore
// let query = "Tur"
// let slab = Slab.create 50

// fuzzyMatch slab query "Saint-Jacques-sur-Darnétal" |> printResult "Saint-Jacques-sur-Darnétal"
// for city in Cities.fr do
//     fuzzyMatch slab query city |> printResult city

// let c = Cities.fr
// let q = "Jaq"
// let slab = Slab.create 200

// c
// |> Array.choose (fun city ->
//     match fuzzyMatch slab q city with
//     | Some res -> Some (city, res)
//     | None -> None
// )
// |> Array.sortByDescending (snd >> fst)
// |> Array.take 10
// |> Array.iter (fst >> printfn "%s")

// let c = "Visual Studio Code"
// let q = "vscode"
// let slab = Slab.create 50
// let res = fuzzyMatch slab q c

// match res with
// | None -> ()
// | Some (s, path) ->
//     printfn "%s: %i" c s
//     c |> Seq.iteri (fun i _ ->
//         match path[i] with
//         | true -> printf "^"
//         | false -> printf " "
//     )
//     printfn ""
