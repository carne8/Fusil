module Fusil

// Reimplementation of the fzf algorithm
// https://github.com/junegunn/fzf/blob/master/src/algo/algo.go

open Fusil
open FsToolkit.ErrorHandling

open System
open System.Collections.Generic

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
        Array2D.init<int>
            (int CharClass.CharNumber + 1)
            (int CharClass.CharNumber + 1)
            (fun i j -> bonusFor (enum<CharClass> i) (enum<CharClass> j))

let fuzzyMatch (query: string) (candidate: string) =
    option {
        let query = query.ToLowerInvariant()
        let candidate = candidate.ToLowerInvariant()

        /// Query length
        let m = query.Length
        /// Candidate length
        let n = candidate.Length
        if m > n then do! None

        // Phase 1: Assign bonus to each character of the candidate
        // and check if all query characters exist in the candidate
        let bonuses = Array.zeroCreate<int> n

        let mutable prevCharClass = int CharClass.initialCharClass
        let mutable queryCharIdx = 0
        for i in 0..n - 1 do
            let charClass = candidate[i] |> CharClass.ofChar |> int
            bonuses[i] <- Bonus.matrix[prevCharClass, charClass]

            if queryCharIdx < m && query[queryCharIdx] = candidate[i] then
                if queryCharIdx = 0 then
                    bonuses[i] <- bonuses[i] * Bonus.firstCharMultiplier

                if queryCharIdx < m then
                    queryCharIdx <- queryCharIdx+1

            prevCharClass <- charClass

        // Prevent character omission
        if queryCharIdx <> m then do! None

        // Phase 2: Create the scores matrix
        let scoreMatrix =
            Array2D.create<{| Score: int; Gap: int; Consecutive: int |}>
                (m + 1)
                (n + 1)
                {| Score = 0; Gap = 0; Consecutive = 0 |}
        let traceback = Dictionary(m * n)

        let mutable bestScore = 0
        let mutable bestPos = 0, 0

        for i in 1..m do
            for j in 1..n do
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
                    if query[i-1] = candidate[j-1] then // Is match
                        let score = diag.Score + Bonus.scoreMatch
                        let mutable bonus = bonuses[j-1]
                        consecutive <- diag.Consecutive + 1

                        if consecutive > 1 then // If extending the chunk
                            /// The positional bonus of the first character of the chunk
                            let firstBonus = bonuses[j-diag.Consecutive]
                            if bonus >= Bonus.boundary && bonus > firstBonus then // Break consecutive chunk
                                consecutive <- 1
                            else
                                bonus <- Int32.Max(bonus, Int32.Max(Bonus.consecutive, firstBonus))

                        // Choose between starting a new chunk or extending the current
                        if score + bonus < s2 then
                            consecutive <- 0
                            bonuses[j-1]
                        else
                            score + bonus
                    else
                        0

                // Choose the best score
                let score = Int32.Max(Int32.Max(s1, s2), 0)
                scoreMatrix[i, j] <-
                    {| Score = score
                       Gap =
                        if s1 < s2 then // If left
                            gapPenalty
                        else
                            0
                       Consecutive = consecutive |}

                if score > 0 then
                    if score >= bestScore then
                        bestScore <- score
                        bestPos <- i, j

                    // Traceback
                    if score > left.Score && score > diag.Score then
                        traceback.Add((i, j), (true, (i-1, j-1)))
                    else
                        traceback.Add((i, j), (false, (i, j-1)))

        // Phase 3: Backtrack
        let rec tracePath (i, j) (matchingPos: bool array) = // I love recursive functions
            let point = scoreMatrix[i, j]

            match i = 0 || j = 0 || point.Score = 0 with
            | true -> matchingPos
            | false ->
                let isMatch, prev = traceback[i, j]
                if isMatch then matchingPos[j-1] <- true
                tracePath prev matchingPos

        let matchingCharacters = tracePath bestPos (Array.zeroCreate<bool> n)
        return bestScore, matchingCharacters
    }
