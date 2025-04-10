namespace Fusil

type internal CharClass =
    | CharWhite = 0
    | CharNonWord = 1
    | CharDelimiter = 2
    | CharLower = 3
    | CharUpper = 4
    | CharLetter = 5
    | CharNumber = 6

module internal CharClass =
    open System

    /// highest ASCII code point
    let [<Literal>] MaxASCII = 127
    let [<Literal>] delimiterChars = "/,:;|"
    let [<Literal>] whiteChars = " \t\n\v\f\r\x85\xA0"
    let [<Literal>] initialCharClass = CharClass.CharWhite

    let asciiCharClasses =
        Array.init<CharClass> (MaxASCII + 1) (fun idx ->
            let c = char idx
            if c >= 'a' && c <= 'z' then
                CharClass.CharLower
            elif c >= 'A' && c <= 'Z' then
                CharClass.CharUpper
            elif c >= '0' && c <= '9' then
                CharClass.CharNumber
            elif whiteChars |> Seq.contains c then
                CharClass.CharWhite
            elif delimiterChars |> Seq.contains c then
                CharClass.CharDelimiter
            else
                CharClass.CharNonWord
        )

    let inline ofAscii (c: Char) = asciiCharClasses[int c]
    let ofNonAscii (c: Char) =
        if Char.IsLower c then CharClass.CharLower
        elif Char.IsUpper c then CharClass.CharUpper
        elif Char.IsNumber c then CharClass.CharNumber
        elif Char.IsLetter c then CharClass.CharLetter
        elif Char.IsWhiteSpace c then CharClass.CharWhite
        elif delimiterChars |> Seq.contains c then CharClass.CharDelimiter
        else CharClass.CharNonWord

    let ofChar c =
        match Char.IsAscii c with
        | true -> ofAscii c
        | false -> ofNonAscii c
