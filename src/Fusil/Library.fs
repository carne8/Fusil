module Fusil

// Reimplementation of the fzf algorithm
// https://github.com/junegunn/fzf/blob/master/src/algo/algo.go

open System
open Shared.Char
open Shared.CharNormalization
open Shared.Array2D
open Shared.Slab

/// Contains bonuses and penalties (negative bonus)
module private Bonus = // Copied from fzf code
    let [<Literal>] scoreMatch = 16s
    let [<Literal>] scoreGapStart = -3s
    let [<Literal>] scoreGapExtension = -1s

    // We prefer matches at the beginning of a word, but the bonus should not be
    // too great to prevent the longer acronym matches from always winning over
    // shorter fuzzy matches. The bonus point here was specifically chosen that
    // the bonus is cancelled when the gap between the acronyms grows over
    // 8 characters, which is approximately the average length of the words found
    // in web2 dictionary and my file system.
    let [<Literal>] boundary = scoreMatch / 2s

    // Although bonus point for non-word characters is non-contextual, we need it
    // for computing bonus points for consecutive chunks starting with a non-word
    // character.
    let [<Literal>] bonusNonWord = scoreMatch / 2s

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
    let [<Literal>] firstCharMultiplier = 2s

    // Extra bonus for word boundary after whitespace character or beginning of the string
    let [<Literal>] bonusBoundaryWhite = boundary + 2s

    // Extra bonus for word boundary after slash, colon, semicolon, and comma
    let [<Literal>] bonusBoundaryDelimiter = boundary + 2s

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
                | _ -> 0s

    // A minor optimization that can give yet another 5% performance boost
    let matrix =
        Array2D.init
            (int CharClass.CharNumber + 1)
            (int CharClass.CharNumber + 1)
            (fun i j -> bonusFor (enum<CharClass> i) (enum<CharClass> j))

#if DEBUG && !FABLE_COMPILER
let private debug (scoreMatrix: Array2D<int16>) (query: char array) (candidate: char array) =
    printf "         "
    candidate |> Seq.iter (printf "%c    ")
    printfn ""

    for i = 0 to scoreMatrix.Height - 1 do
        printf "%c " query[i]

        for j = 0 to scoreMatrix.Width - 1 do
            let s = scoreMatrix[i, j]
            if s <> 0s then
                Console.ForegroundColor <- ConsoleColor.Yellow
            printf "%s  " <| s.ToString "000"
            if s <> 0s then
                Console.ForegroundColor <- ConsoleColor.White
        printfn ""
#endif

let fuzzyMatch
    (slab: Slab) // Arrays used to store computation data
    caseSensitive // Does case matter
    normalize // Transform accentuated characters to ascii
    (queryStr: string)
    (candidateStr: string)
    =
    let query = queryStr.ToCharArray()
    let candidate = candidateStr.ToCharArray()

    /// Query length
    let m = query.Length
    /// Candidate length
    let n = candidate.Length
    if m > n then
        None
    else

    if m = 0 then
        None
    else

    // Phase 1:
    //  Assign bonus to each character of the candidate
    //  Check if all query characters exist in the candidate
    //  Find the first appearance index of each query character in the candidate
    //  Find the last matching character index

    // This is the offset used to allocate the different arrays in the slab
    let slabOffset = 0

    let mutable slabOffset, bonuses = slab |> Slab.alloc16 slabOffset n
    let mutable slabOffset, firstCharOccurrence = slab |> Slab.alloc16 slabOffset m
    let mutable lastMatchingIndex = 0

    let slabOffset, scoreMatrix = slab |> Array2D.allocFromSlab m (n + 1) slabOffset
    let _slabOffset, consecutiveMatrix = slab |> Array2D.allocFromSlab m (n + 1) slabOffset
    scoreMatrix[0, 0] <- 0s // Needed for correct backtracking

    let mutable bestScore = 0s
    let mutable bestPos = 0, 0
    let mutable inGap = false

    let mutable prevCharClass = CharClass.initialCharClass
    let mutable queryCharIdx = 0
    let mutable prevCharScore = 0s
    let mutable i = 0
    while i <= n-1 && queryCharIdx <= m do
        let mutable charClass = CharClass.CharWhite
        if candidate[i] |> Char.isAscii then
            charClass <- candidate[i] |> CharClass.ofAscii

            if not caseSensitive && charClass = CharClass.CharUpper then
                candidate[i] <- candidate[i] + char 32
        else
            charClass <- candidate[i] |> CharClass.ofNonAscii

            if not caseSensitive && charClass = CharClass.CharUpper then
                candidate[i] <- candidate[i] |> Char.ToLower

            if normalize then
                candidate[i] <- candidate[i] |> Char.normalize

        let bonus = Bonus.matrix[int prevCharClass, int charClass]
        bonuses[i] <- bonus

        if queryCharIdx < m && query[queryCharIdx] = candidate[i] then // Characters match
            // Update last matching index
            lastMatchingIndex <- i

            if queryCharIdx = 0 then
                bonuses[i] <- bonus * Bonus.firstCharMultiplier

            // Set first occurrence
            firstCharOccurrence[queryCharIdx] <- int16 i
            queryCharIdx <- queryCharIdx+1

        // Set score if match
        if candidate[i] = query[0] then
            let score = Bonus.scoreMatch + bonus*Bonus.firstCharMultiplier
            scoreMatrix[0, i+1] <- score
            consecutiveMatrix[0, i+1] <- 1s
            inGap <- false
            prevCharScore <- score

            if score >= bestScore then
                bestScore <- score
                bestPos <- 0, i+1
        else
            let gapPenalty =
                match inGap with
                | false -> Bonus.scoreGapStart
                | true -> Bonus.scoreGapExtension
            let score = max 0s (prevCharScore + gapPenalty)
            scoreMatrix[0, i+1] <- score
            prevCharScore <- score

            consecutiveMatrix[0, i+1] <- 1s
            inGap <- true

        prevCharClass <- charClass
        i <- i+1

    // Prevent character omission
    if queryCharIdx <> m then
        None
    else

    // Phase 2: Compute the scores matrix
    for i = 1 to m-1 do
        inGap <- false
        scoreMatrix[i, 0] <- 0s

        for j = int firstCharOccurrence[i]+1 to lastMatchingIndex+1 do // Prevent useless computing
            let diagScore = scoreMatrix[i-1, j-1]
            let diagConsecutive = consecutiveMatrix[i-1, j-1]
            let leftScore = scoreMatrix[i, j-1]

            let mutable consecutive = 0s

            // Compute the score if choosing left -> s2
            let gapPenalty =
                match inGap with
                | false -> Bonus.scoreGapStart
                | true -> Bonus.scoreGapExtension
            let s2 = leftScore + gapPenalty

            // Compute the score if choosing diagonal -> s1
            let s1 =
                if query[i] = candidate[j-1] then // Is match
                    let score = diagScore + Bonus.scoreMatch
                    let mutable bonus = bonuses[j-1]
                    consecutive <- diagConsecutive + 1s

                    if consecutive > 1s then // If extending the chunk
                        /// The positional bonus of the first character of the chunk
                        let firstBonus = bonuses[j-int diagConsecutive]
                        if bonus >= Bonus.boundary && bonus > firstBonus then // Break consecutive chunk
                            consecutive <- 1s
                        else
                            bonus <- Math.Max(bonus, Math.Max(Bonus.consecutive, firstBonus))

                    // Choose between starting a new chunk or extending the current
                    if score + bonus < s2 then
                        consecutive <- 0s
                        bonuses[j-1]
                    else
                        score + bonus
                else
                    0s

            // Choose the best score
            let score = max (max s1 s2) 0s
            scoreMatrix[i, j] <- score
            consecutiveMatrix[i, j] <- consecutive

            inGap <- s1 < s2

            if score >= bestScore then
                bestScore <- score
                bestPos <- i, j

    #if DEBUG && !FABLE_COMPILER
    debug scoreMatrix query candidate
    #endif

    // Phase 3: Backtrack
    let mutable i = fst bestPos
    let mutable j = snd bestPos

    let mutable matchingCharacter =
        #if FABLE_COMPILER
        Array.zeroCreate n
        #else
        GC.AllocateUninitializedArray<bool> n
        #endif

    while i >= 0 && j >= 0 do
        let diagScore = if i = 0 then 0s else scoreMatrix[i, j-1]
        let leftScore = scoreMatrix[i, j-1]
        let s = scoreMatrix[i, j]

        if s > leftScore && s > diagScore then
            matchingCharacter[j-1] <- true
            i <- i-1
        j <- j-1

    Some (bestScore, matchingCharacter)