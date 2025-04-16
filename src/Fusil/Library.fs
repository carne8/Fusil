module Fusil

// Reimplementation of the fzf algorithm
// https://github.com/junegunn/fzf/blob/master/src/algo/algo.go

open Shared.CharClass
open Shared.Array2D
open FsToolkit.ErrorHandling

open System

/// Contains bonuses and penalties (negative bonus)
module private Bonus = // Copied from fzf code
    let [<Literal>] scoreMatch = 16
    let [<Literal>] scoreGapStart = -3
    let [<Literal>] scoreGapExtension = -1

    // We prefer matches at the beginning of a word, but the bonus should not be
    // too great to prevent the longer acronym matches from always winning over
    // shorter fuzzy matches. The bonus point here was specifically chosen that
    // the bonus is cancelled when the gap between the acronyms grows over
    // 8 characters, which is approximately the average length of the words found
    // in web2 dictionary and my file system.
    let [<Literal>] boundary = scoreMatch / 2

    // Although bonus point for non-word characters is non-contextual, we need it
    // for computing bonus points for consecutive chunks starting with a non-word
    // character.
    let [<Literal>] bonusNonWord = scoreMatch / 2

    // Edge-triggered bonus for matches in camelCase words.
    // Compared to word-boundary case, they don't accompany single-character gaps
    // (e.g. FooBar vs. foo-bar), so we deduct bonus point accordingly.
    let [<Literal>] bonusCamel123 = boundary + scoreGapExtension

    // Minimum bonus point given to characters in consecutive chunks.
    // Note that bonus points for consecutive matches shouldn't have needed if we
    // used fixed match score as in the original algorithm.
    let [<Literal>] consecutive = -(scoreGapStart + scoreGapExtension)

    // The first character in the typed pattern usually has more significance
    // than the rest so it's important that it appears at special positions where
    // bonus points are given, e.g. "to-go" vs. "ongoing" on "og" or on "ogo".
    // The amount of the extra bonus should be limited so that the gap penalty is
    // still respected.
    let [<Literal>] firstCharMultiplier = 2

    // Extra bonus for word boundary after whitespace character or beginning of the string
    let [<Literal>] bonusBoundaryWhite = boundary + 2

    // Extra bonus for word boundary after slash, colon, semicolon, and comma
    let [<Literal>] bonusBoundaryDelimiter = boundary + 2

    let bonusFor prevClass class' =
        let b =
            if class' > CharClass.CharNonWord then
                match prevClass with
                // Word boundary after whitespace
                | CharClass.CharWhite -> Some bonusBoundaryWhite
                // Word boundary after a delimiter character
                | CharClass.CharDelimiter -> Some bonusBoundaryDelimiter
                // Word boundary
                | CharClass.CharNonWord -> Some boundary
                | _ -> None
            else
                None

        match b with
        | Some b -> b
        | _ ->
            if prevClass = CharClass.CharLower && class' = CharClass.CharUpper
               || prevClass <> CharClass.CharNumber && class' = CharClass.CharNumber then
                // camelCase letter123
                bonusCamel123
            else
                match class' with
                | CharClass.CharNonWord | CharClass.CharDelimiter -> bonusNonWord
                | CharClass.CharWhite -> bonusBoundaryWhite
                | _ -> 0

    // A minor optimization that can give yet another 5% performance boost
    let matrix =
        Array2D.init
            (int CharClass.CharNumber + 1)
            (int CharClass.CharNumber + 1)
            (fun i j -> bonusFor (enum<CharClass> i) (enum<CharClass> j))

[<Struct>]
type ScoreMatrixPoint =
    { Score: int
      Gap: int
      Consecutive: int }

    static member Default =
        { Score = 0
          Gap = 0
          Consecutive = 0 }

let fuzzyMatch (query: string) (candidate: string) =
    option {
        let queryLower = query.ToLowerInvariant()
        let candidateLower = candidate.ToLowerInvariant()

        /// Query length
        let m = query.Length
        /// Candidate length
        let n = candidate.Length
        if m > n then do! None

        // Phase 1: Assign bonus to each character of the candidate
        // and check if all query characters exist in the candidate
        // Also find the first appearence index of each query character in the candidate
        let bonuses = Array.zeroCreate n
        let firstCharOccurence = Array.zeroCreate m

        let mutable prevCharClass = int CharClass.initialCharClass
        let mutable queryCharIdx = 0
        for i = 0 to n - 1 do
            let charClass = candidate[i] |> CharClass.ofChar |> int
            bonuses[i] <- Bonus.matrix[prevCharClass, charClass]

            if queryCharIdx < m && queryLower[queryCharIdx] = candidateLower[i] then
                if queryCharIdx = 0 then
                    bonuses[i] <- bonuses[i] * Bonus.firstCharMultiplier

                if queryCharIdx < m then
                    firstCharOccurence[queryCharIdx] <- i
                    queryCharIdx <- queryCharIdx+1

            prevCharClass <- charClass

        // Prevent character omission
        if queryCharIdx <> m then do! None

        // Phase 2: Create the scores matrix
        let scoreMatrix = Array2D.create (m + 1) (n + 1) ScoreMatrixPoint.Default

        let mutable bestScore = 0
        let mutable bestPos = 0, 0

        for i = 1 to m do
            for j = firstCharOccurence[i-1] + 1 to n do // Prevent useless computing
                let diag = scoreMatrix[i-1, j-1]
                let left = scoreMatrix[i, j-1]

                let mutable consecutive = 0

                // Compute the score if choosing left -> s2
                let gapPenalty =
                    match left.Gap with
                    | 0 -> Bonus.scoreGapStart
                    | _ -> Bonus.scoreGapExtension
                let s2 = left.Score + gapPenalty

                // Compute the score if choosing diagonal -> s1
                let s1 =
                    if queryLower[i-1] = candidateLower[j-1] then // Is match
                        let score = diag.Score + Bonus.scoreMatch
                        let mutable bonus = bonuses[j-1]
                        consecutive <- diag.Consecutive + 1

                        if consecutive > 1 then // If extending the chunk
                            /// The positional bonus of the first character of the chunk
                            let firstBonus = bonuses[j-diag.Consecutive]
                            if bonus >= Bonus.boundary && bonus > firstBonus then // Break consecutive chunk
                                consecutive <- 1
                            else
                                bonus <- Math.Max(bonus, Math.Max(Bonus.consecutive, firstBonus))

                        // Choose between starting a new chunk or extending the current
                        if score + bonus < s2 then
                            consecutive <- 0
                            bonuses[j-1]
                        else
                            score + bonus
                    else
                        0

                // Choose the best score
                let score = Math.Max(Math.Max(s1, s2), 0)
                scoreMatrix[i, j] <-
                    { Score = score
                      Gap =
                        if s1 < s2 then // If left
                            gapPenalty
                        else
                            0
                      Consecutive = consecutive }

                if score >= bestScore then
                    bestScore <- score
                    bestPos <- i, j

        // --- DEBUG
        // printf "         "
        // candidate |> Seq.iter (printf "%c    ")
        // printfn ""

        // for i = 0 to scoreMatrix.Height - 1 do
        //     if i > 0 then
        //         printf "%c " query[i - 1]
        //     else
        //         printf "  "

        //     for j = 0 to scoreMatrix.Width - 1 do
        //         let s = scoreMatrix[i, j].Score
        //         if s <> 0 then
        //             Console.ForegroundColor <- ConsoleColor.Yellow
        //         printf "%s  " <| s.ToString "000"
        //         if s <> 0 then
        //             Console.ForegroundColor <- ConsoleColor.White
        //     printfn ""
        // --- DEBUG

        // Phase 3: Backtrack
        let mutable i = fst bestPos
        let mutable j = snd bestPos
        let point () = scoreMatrix[i, j]
        let mutable matchingCharacter = Array.zeroCreate<bool> n

        while i > 0 do
            let diag = scoreMatrix[i-1, j-1]
            let left = scoreMatrix[i, j-1]
            let s = point().Score

            if s > left.Score && s > diag.Score then
                matchingCharacter[j-1] <- true
                i <- i-1
            j <- j-1

        return bestScore, matchingCharacter
    }
