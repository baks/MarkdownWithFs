namespace MarkdownWithFs

open Markdown
open FParsec
open FParsec.CharParsers

module WithParsec =

    let explode (str:string) = str |> List.ofSeq
    let implode (xs:char list) = new string(xs |> Array.ofList)
 
    let getRestOfInput : Parser<string,'u> = 
        fun stream ->
            Reply(stream.ReadCharsOrNewlinesWhile((fun c -> true), false))
 
    let delimitedParser (opening:char list) (closing:char list) = 
        let openingStr = System.String.Join("", opening)
        let closingStr = System.String.Join("", closing)
 
        between <| pstring openingStr <| pstring closingStr <| manySatisfy (fun c -> not <| List.contains c closing)
 
    let delimitedParser' (opening:char list) (closing:char list) parser = 
        let openingStr = System.String.Join("", opening)
        let closingStr = System.String.Join("", closing)
 
        between <| pstring openingStr <| pstring closingStr <| parser
 
    let (|Delimited|_|) (opening:'a list) (closing:char list) input = 
        let openingStr = System.String.Join("", opening)
        let closingStr = System.String.Join("", closing)
 
        match run (between <| pstring openingStr <| pstring closingStr <| manySatisfy (fun c -> not <| List.contains c closing) .>>. getRestOfInput) input with
        | Failure (_,_,_) -> None
        | Success (res, _, _) -> Some res
 
    let parseSpans' text =    
        let rec loop xs prev = seq {
            let toLiteral' = seq {
                if prev <> [] then yield prev |> List.rev |> implode |> Literal
            }
 
            match xs with
            | Delimited ['`'] ['`'] (inlineBody, after) -> 
                yield! toLiteral'
                yield inlineBody |> InlineCode
                yield! loop after []
            | Delimited ['*';'*'] ['*';'*'] (strongBody, after) ->
                yield! toLiteral'
                yield loop strongBody [] |> List.ofSeq |> Strong
                yield! loop after []
            | Delimited ['['] [']'] (linkTitle, afterTitle) ->
                match afterTitle with
                | Delimited ['('] [')'] (link, afterLink) -> 
                    yield! toLiteral'
                    yield (loop linkTitle [] |> List.ofSeq, link) |> HyperLink
                    yield! loop afterLink []
                | _ -> yield! toLiteral'
            | "" -> yield! toLiteral'
            | _ -> yield! loop (xs.Substring(1, xs.Length - 1)) ((xs.Substring(0,1).[0])::prev)
        }
 
        (text, []) ||> loop
 
    let (<!>) (p: Parser<_,_>) label : Parser<_,_> =
        fun stream ->
            printfn "%A: Entering %s" stream.Position label
            let reply = p stream
            printfn "%A: Leaving %s (%A)" stream.Position label reply.Status
            reply
 
 
    let parseMarkdown text =
        let markdownSpans, markdownSpansRef = createParserForwardedToRef<MarkdownSpans, unit>()
        let markdownSpansTillNewline, markdownSpansTillNewlineRef = createParserForwardedToRef<MarkdownSpans, unit>()
        let markdownSpan, markdownSpanRef = createParserForwardedToRef<MarkdownSpan, unit>()
 
        let const' k _ = k
        let blankLines = newline >>. many (satisfy (fun c -> c = ' ')) >>. newline >>. preturn ()
 
        let literalStrong = many1 (satisfy (fun c -> c <> '*')) |>> implode |>> Literal
        let literalEmphasis = many1 (satisfy (fun c -> c<> '_')) |>> implode |>> Literal
 
        let inlineCode = delimitedParser ['`'] ['`'] |>> InlineCode
        let emphasis = pstring "_" >>. manyTill (literalEmphasis <|> markdownSpan) (pstring "_") <|>  ((pstring "*") >>. manyTill markdownSpan ((notFollowedBy spaces) >>. pstring "*")) |>> Emphasis
        let strong = pstring "**" >>. manyTill (literalStrong <|> markdownSpan) (pstring "**") |>> Strong
        
        let ts = next2CharsSatisfy (fun f s -> not <| List.contains f ['`'; '*'; '_'])
        let escape =  (many (satisfy Text.IsWhitespace) .>> pchar '*' .>>. many1 (satisfy Text.IsWhitespace) |>> (fun (s,a) -> sprintf "%s*%s" (implode s) (implode a))) |>> Literal
        
        
        let literalHyperLink = many1 (satisfy (fun c -> c <> ']')) |>> implode |>> Literal
        let hyperlink = pstring "[" >>. manyTill (literalHyperLink <|> markdownSpan) (pstring "]") .>>. skipSatisfy System.Char.IsWhiteSpace .>>. delimitedParser ['('] [')'] |>> (fun ((title,_), link) -> HyperLink(title,link))
        let hardLineBreak = pchar ' ' >>. pchar ' ' >>. newline |>> const' HardLineBreak
        let literal' = notFollowedByString "> " >>.  manyTill (satisfy (fun c -> true)) (lookAhead blankLines) |>> implode |>> Literal
        let literal =  notFollowedByString "> " >>. manyTill (satisfy (fun c -> true)) (lookAhead markdownSpan <|> (lookAhead blankLines >>. preturn (MarkdownSpan.Literal ""))) |>> implode |>> Literal
        let literalEof = notFollowedByString "> " >>. manyTill (satisfy (fun c -> true)) (lookAhead blankLines <|> eof) |>> implode |>> Literal
 
        let literalNewline' = manyTill (satisfy (fun c -> true)) (lookAhead newline) |>> implode |>> Literal
        let literalNewlineEof = manyTill (satisfy (fun c -> true)) (lookAhead (newline >>. preturn()) <|> eof) |>> implode |>> Literal

        markdownSpanRef := choice <| ([inlineCode;strong;emphasis;hyperlink] |> List.map attempt)
        markdownSpansRef := manyTill (markdownSpan <|> (attempt literal <|> (attempt literal' <!> "literal") <|> attempt literalEof)) (lookAhead blankLines <|> eof)
        markdownSpansTillNewlineRef := manyTill (markdownSpan <|> (attempt literalNewline' <|> attempt literal <|> attempt literalNewlineEof)) (lookAhead (newline >>. preturn ()) <|> eof)

        let setextFirstLevel = markdownSpansTillNewline .>> (newline) .>> (many1 (satisfy (fun c -> c = '=' ))) .>> (lookAhead newline)
        let setextSecondLevel = markdownSpansTillNewline .>> (newline) .>> (many1 (satisfy (fun c -> c = '-' ))) .>> (lookAhead newline)
        let setextSep = setextFirstLevel <|> setextSecondLevel |>> fun x -> Heading(1,x)
        let heading1 = pstring "# " >>. markdownSpans |>> fun x -> Heading(1,x)
        let heading2 = pstring "## " >>. markdownSpans |>> fun x -> Heading(2,x)
        let codeBlock = (preturn (fun () -> printfn "code block start") >>. (newline >>. pstring "    ") <|> pstring "    " .>> preturn (fun () -> printfn "4 spaces matched") >>. many1 (satisfy (fun c -> c <> '\r' && c <> '\n')) .>> preturn (fun() -> printfn "many1 matched") |>> implode)
        let codeBlockManyLines = manyTill ((codeBlock) <!> "single code block") ((lookAhead blankLines) <!> "endOfCb") |>> CodeBlock
        let emptyLine = manyTill (satisfy Text.IsWhitespace) newline
        let paragraphs'' = spaces >>. markdownSpansTillNewline |>> Paragraph
        let paragraphs' = spaces >>. sepEndBy (attempt setextSep <|> attempt heading2 <|> attempt heading1 <|> (attempt codeBlockManyLines <!> "codeBlock") <|> (markdownSpans |>> Paragraph)) ((lookAhead (followedByString "> ")) <|> blankLines)
        
        let markdown = manyTill (paragraphs') (eof)
        match run markdown text with
        | ParserResult.Success (r,_,_) -> r |> List.concat
        | ParserResult.Failure(_,_,_) -> failwith "Markdown parsec failure"
        