module Fusil

// Reimplementation of the fzf algorithm
// https://github.com/junegunn/fzf/blob/master/src/algo/algo.go

open System
open Shared.Char
open Shared.CharNormalization
open Shared.Array2D
open Shared.Slab
open Shared
open System.Collections.Generic

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
            if class' > CharClass.NonWord then
                match prevClass with
                // Word boundary after whitespace
                | CharClass.White -> Some bonusBoundaryWhite
                // Word boundary after a delimiter character
                | CharClass.Delimiter -> Some bonusBoundaryDelimiter
                // Word boundary
                | CharClass.NonWord -> Some boundary
                | _ -> None
            else
                None

        match b with
        | Some b -> b
        | _ ->
            if prevClass = CharClass.Lower && class' = CharClass.Upper
               || prevClass <> CharClass.Number && class' = CharClass.Number then
                // camelCase letter123
                bonusCamel123
            else
                match class' with
                | CharClass.NonWord | CharClass.Delimiter -> bonusNonWord
                | CharClass.White -> bonusBoundaryWhite
                | _ -> 0s

    // A minor optimization that can give yet another 5% performance boost
    let matrix =
        Array2D.init
            (int CharClass.Number + 1)
            (int CharClass.Number + 1)
            (fun i j -> bonusFor (enum<CharClass> i) (enum<CharClass> j))

#if DEBUG && !FABLE_COMPILER
// let private debug (scoreMatrix: Array2D<int16>) (query: char array) (candidate: char array) =
//     printf "         "
//     candidate |> Seq.iter (printf "%c    ")
//     printfn ""

//     for i = 0 to scoreMatrix.Height - 1 do
//         printf "%c " query[i]

//         for j = 0 to scoreMatrix.Width - 1 do
//             let s = scoreMatrix[i, j]
//             if s <> 0s then
//                 Console.ForegroundColor <- ConsoleColor.Yellow
//             printf "%s  " <| s.ToString "000"
//             if s <> 0s then
//                 Console.ForegroundColor <- ConsoleColor.White
//         printfn ""
let private debug (T: int Collections.Generic.IList) (pattern: char array) (F: int32 Collections.Generic.IList) (lastIdx: int) (H: int16 Collections.Generic.IList) (C: int16 Collections.Generic.IList) =
    let width = lastIdx - int F[0] + 1

    for i in 0 .. pattern.Length - 1 do
        let f = int F[i]
        let I = i * width

        if i = 0 then
            printf "  "
            for j in f .. lastIdx do
                printf " %c " (char T[j])
            printfn ""

        printf "%c " pattern[i]

        for idx in int F[0] .. f - 1 do
            printf " 0 "

        for idx in f .. lastIdx do
            let h = H[i * width + idx - int F[0]]
            printf "%2d " h
        printfn ""

        printf "  "
        for idx = 0 to width - 1 do
            let col = idx + int F[0]
            let p =
                if col < int F[i] then 0s
                else C[I + idx]

            if p > 0s then
                printf "%2d " p
            else
                printf "   "
        printfn ""
#endif

let posSet withPos (length: int) =
    if withPos then
        HashSet length
    else
        null

module GoSimilar =
    let copyRunes (arr: int32 Collections.Generic.IList) (str: string) =
        let mutable enumerator = str.EnumerateRunes()
        let mutable i = 0
        while enumerator.MoveNext() do
            arr[i] <- enumerator.Current.Value
            i <- i+1

[<Struct>]
type Result =
    { Start: int
      End: int
      Score: int16
      MatchingPositions: HashSet<int> }

let fuzzyMatch caseSensitive normalize withPos (slab: Slab) (pattern: char array) (input: string) =
    // Assume that pattern is given in lowercase if case-insensitive.
    // First check if there's a match and calculate bonus for each position.
    // If the input string is too long, consider finding the matching chars in
    // this phase as well (non-optimal alignment).
    let M = pattern.Length
    if M = 0 then
        Some { Start = 0; End = 0; Score = 0s; MatchingPositions = null }
    else

    let N = input.Length
    if M > N then
        None
    else

    // Since O(nm) algorithm can be prohibitively expensive for large input,
    // we fall back to the greedy algorithm.
    if N*M > slab.i16.Length then
        None // TODO: fallback on a faster algorithm
    else

    // TODO: build phase 1
    // Phase 1. Optimized search for ASCII string
    let minIdx, maxIdx = 0, input.Length

    // Reuse pre-allocated integer slice to avoid unnecessary sweeping of garbages
    let mutable offset16 = 0
    let mutable offset32 = 0
    let mutable offset16, H0 = slab |> Slab.alloc16 offset16 N
    let mutable offset16, C0 = slab |> Slab.alloc16 offset16 N
    // Bonus point for each position
    let mutable offset16, B = slab |> Slab.alloc16 offset16 N
    // The first occurrence of each character in the pattern
    let mutable offset32, F = slab |> Slab.alloc32 offset32 M
    // Rune array
    let mutable _, T = slab |> Slab.alloc32 offset32 N
    input |> GoSimilar.copyRunes T

    // Phase 2: Calculate bonus for each point
    let mutable maxScore, maxScorePos = 0s, 0
    let mutable pidx, lastIdx = 0, 0
    let mutable pchar0, pchar, prevH0, prevClass, inGap = pattern[0], pattern[0], 0s, CharClass.initialCharClass, false

    let mutable off = 0
    let mutable finished = false
    while off <= N-1 && not finished do
        let mutable char' = T[off]
        let mutable class' = CharClass.White
        if char' |> Rune.isAscii then
            class' <- CharClass.asciiCharClasses[char']
            if not caseSensitive && class' = CharClass.Upper then
                char' <- char' + 32
                T[off] <- char'
        else
            class' <- char' |> CharClass.ofNonAscii
            if not caseSensitive && class' = CharClass.Upper then
                char' <- char' |> Rune.toLower

            if normalize then
                char' <- CharNormalization.Rune.normalize char'

            T[off] <- char'

        let bonus = Bonus.matrix[int prevClass, int class']
        B[off] <- bonus
        prevClass <- class'

        if char char' = pchar then
            if pidx < M then
                F[pidx] <- off
                pidx <- pidx+1
                pchar <- pattern[min pidx (M-1)]
            lastIdx <- off

        if char char' = pchar0 then
            let score = Bonus.scoreMatch + bonus*Bonus.firstCharMultiplier
            H0[off] <- score
            C0[off] <- 1s

            if M = 1 && score > maxScore then
                maxScore <- score
                maxScorePos <- off
                if bonus >= Bonus.boundary then
                    finished <- true

            inGap <- false
        else
            H0[off] <-
                if inGap then
                    max (prevH0 + Bonus.scoreGapExtension) 0s
                else
                    max (prevH0 + Bonus.scoreGapStart) 0s

            C0[off] <- 0s
            inGap <- true

        prevH0 <- H0[off] // TODO: Benchmark
        off <- off+1

    if pidx <> M then // Input doesn't contain pattern
        None
    else

    if M = 1 then
        if withPos then
            Some { Start = minIdx + maxScorePos
                   End = minIdx + maxScorePos
                   Score = maxScore
                   MatchingPositions = HashSet (minIdx + maxScorePos) }
        else
            Some { Start = minIdx + maxScorePos
                   End = minIdx + maxScorePos
                   Score = maxScore
                   MatchingPositions = null }
    else

    // Phase 3: Fill in score matrix
    // Unlike the original algorithm, we do not allow omission.
    let f0 = F[0]
    let width = lastIdx - f0 + 1
    let mutable offset16, H = slab |> Slab.alloc16 offset16 (width*M)
    // copy(H, H0[f0:lastIdx+1]) // TODO: Consider usage of span
    for i = f0 to lastIdx do
        H[i] <- H0[i]

    // Possible length of consecutive chunk at each position.
    let mutable _, C = slab |> Slab.alloc16 offset16 (width*M)
    // TODO: Consider usage of span
    for i = f0 to lastIdx do
        C[i] <- C0[i]

    let FSub = F.Slice(1, F.Count - 1)
    let PSub = ArraySegment(pattern, 1, FSub.Count)
    for off = 0 to FSub.Count - 1 do
        let f = FSub[off]
        let pchar = PSub[off]
        let pidx = off + 1
        let row = pidx * width
        let mutable inGap = false
        let TSub = T.Slice(f, lastIdx+1 - f)
        let BSub = B.Slice(f).Slice(0, TSub.Count)
        let mutable CSub = C.Slice(row+f-f0).Slice(0, TSub.Count)
        let CDiag = C.Slice(row+f-f0-1-width).Slice(0, TSub.Count)
        let mutable HSub = H.Slice(row+f-f0).Slice(0, TSub.Count)
        let HDiag = H.Slice(row+f-f0-1-width).Slice(0, TSub.Count)
        let mutable HLeft = H.Slice(row+f-f0-1).Slice(0, TSub.Count)
        HLeft[0] <- 0s

        for off = 0 to TSub.Count - 1 do
            let c = char TSub[off]
            let col = off + f
            let mutable consecutive = 0s

            let s2 =
                if inGap then
                    HLeft[off] + Bonus.scoreGapExtension
                else
                    HLeft[off] + Bonus.scoreGapStart

            let s1 =
                if pchar = c then
                    let score = HDiag[off] + Bonus.scoreMatch
                    let mutable b = BSub[off]
                    consecutive <- CDiag[off] + 1s
                    if consecutive > 1s then
                        let fb = B[col - int consecutive + 1]
                        // Break consecutive chunk
                        if b >= Bonus.boundary && b > fb then
                            consecutive <- 1s
                        else
                            b <- max b (max Bonus.consecutive fb)

                    if score+b < s2 then
                        consecutive <- 0s
                        score + BSub[off]
                    else
                        score + b
                else
                    0s

            CSub[off] <- consecutive
            inGap <- s1 < s2
            let score = max (max s1 s2) 0s
            if pidx = M-1 && score > maxScore then
                maxScore <- score
                maxScorePos <- col
            HSub[off] <- score

    #if DEBUG && !FABLE_COMPILER
    debug T pattern F lastIdx H C
    #endif

    // Phase 4: Backtrace to find the character positions
    let matchingPositions = posSet withPos M
    let mutable j = f0
    if withPos then
        let mutable i = M-1
        j <- maxScorePos
        let mutable preferMatch = true
        let mutable finished = false

        while not finished do
            let I = i * width
            let j0 = j - f0
            let s = H[I+j0]

            let s1 =
                if i > 0 && j >= F[i] then
                    H[I-width+j0-1]
                else
                    0s

            let s2 =
                if j > F[i] then
                    H[I+j0-1]
                else
                    0s

            if s > s1 && (s > s2 || s = s2 && preferMatch) then
                j+minIdx |> matchingPositions.Add |> ignore
                if i = 0 then
                    j <- j+1 // To cancel the `j <- j-1`
                    finished <- true
                i <- i-1

            preferMatch <- C[I+j0] > 1s || I+width+j0+1 < C.Count && C[I+width+j0+1] > 0s
            j <- j-1

    // Start offset we return here is only relevant when begin tiebreak is used.
    // However finding the accurate offset requires backtracking, and we don't
    // want to pay extra cost for the option that has lost its importance.
    Some { Start = minIdx + j
           End = minIdx + maxScorePos + 1
           Score = maxScore
           MatchingPositions = matchingPositions }