namespace MarkdownWithFs

open Markdown
 
module Parser =
 
    let explode (str:string) = str |> List.ofSeq
    let implode (xs:char list) = new string(xs |> Array.ofList)
 
    let rec parseInlineBody xs acc =
        match xs with
        | '`' :: rest -> acc |> List.rev |> fun x -> Some (x, rest)
        | c :: rest -> parseInlineBody rest (c::acc)
        | [] -> None
 
 
    let parseSpans text =    
        let rec loop xs prev = seq {
            let toLiteral' = seq {
                if prev <> [] then yield prev |> List.rev |> implode |> Literal
            }
 
            match xs with
            | '`' :: rest -> 
                        yield! toLiteral'
                        match parseInlineBody rest [] with
                        | Some (body, afterBody) -> 
                            yield body |> implode |> InlineCode
                            yield! loop afterBody []
                        | None -> yield! []
            | x :: xs -> yield! loop xs (x::prev)
            | [] -> yield! toLiteral'
        }
 
        (text |> explode, []) ||> loop
 
    let (|StartsWith|_|) prefix input =
        let rec startsWith prefixChars xs =
            match prefixChars, xs with
            | char::[], x:: rest when x = char -> Some rest
            | char::prefixRest, x :: rest when x = char -> startsWith prefixRest rest
            | _ -> None  
 
        startsWith prefix input
 
    let rec parseDelimitedBody closing acc input =
        match input with
        | StartsWith closing rest -> Some(acc |> List.rev, rest)
        | c :: rest -> parseDelimitedBody closing (c::acc) rest
        | [] -> None
 
    let (|Delimited|_|) opening closing input = 
        let rec loop xs = 
            match xs with
            | StartsWith opening body -> parseDelimitedBody closing [] body
            | _ -> None
    
        loop input
 
    let (|InlineCode|_|) input =
        match input with
        | Delimited ['`'] ['`'] (inlineBody, after) -> 
            Some (inlineBody |> implode |> MarkdownSpan.InlineCode, after)
        | _ -> 
            None
 
    let (|Strong|_|) loop input =
        match input with
        | Delimited ['*';'*'] ['*';'*'] (strongBody, after) ->
            Some (loop strongBody [] |> List.ofSeq |> MarkdownSpan.Strong, after)
        | Delimited ['_'; '_'] ['_';'_'] (strongBody, after) ->
            Some (loop strongBody [] |> List.ofSeq |> MarkdownSpan.Strong, after)
        | _ ->
            None
 
    let (|Emphasis|_|) loop input =
        match input with
        | Delimited ['*'] ['*'] (emphasisBody, after) ->
            Some (loop emphasisBody [] |> List.ofSeq |> MarkdownSpan.Emphasis, after)
        | Delimited ['_'] ['_'] (emphasisBody, after) ->
            Some (loop emphasisBody [] |> List.ofSeq |> MarkdownSpan.Emphasis, after)
        | _ -> 
            None
 
    let (|Hyperlink|_|) loop input =
        match input with
        | Delimited ['['] [']'] (linkTitle, afterTitle) ->
            match afterTitle with
            | Delimited ['('] [')'] (link, afterLink) -> 
                Some ((loop linkTitle [] |> List.ofSeq, link |> implode) |> HyperLink, afterLink)
            | _ -> None
        | _ -> None
 
    let (|HardLineBreak|_|) input =
        match input with
        | StartsWith [' '; ' '] rest ->
            match rest with
            | StartsWith ['\r';'\n'] after -> Some (MarkdownSpan.HardLineBreak, after)
            | StartsWith ['\r'] after -> Some (MarkdownSpan.HardLineBreak, after)
            | StartsWith ['\n'] after -> Some (MarkdownSpan.HardLineBreak, after)
            | _ -> None
        | _ -> None
 
    let parseSpans' text =    
        let rec loop xs prev = seq {
            let toLiteral' = seq {
                if prev <> [] then yield prev |> List.rev |> implode |> Literal
            }
 
            let processEl loop el = seq {
                yield! toLiteral'
                yield el
                yield! loop
            } 
 
            match xs with
            | InlineCode (ic, after) -> 
                yield! processEl (loop after []) ic
            | Strong loop (strong, after) ->
                yield! processEl (loop after []) strong
            | Emphasis loop (emphasis, after) ->
                yield! processEl (loop after []) emphasis
            | Hyperlink loop (hyperlink, after) ->
                yield! processEl (loop after []) hyperlink
            | HardLineBreak (hlBreak, after) ->
                yield! processEl (loop after []) hlBreak
            | x :: xs -> yield! loop xs (x::prev)
            | [] -> yield! toLiteral'
        }
 
        (text |> explode, []) ||> loop
 
    let parseBlocks text =
        let rec loop xs prev = seq {
            match xs with
            | '\r' :: '\n' :: rest ->
                match rest |> List.takeWhile (fun c -> c <> '\r' || c <> '\n') |> List.forall System.Char.IsWhiteSpace, prev with
                | _ , x when x |> List.forall System.Char.IsWhiteSpace ->
                    yield! loop rest []
                | true, _ ->
                    let after = rest |> List.skipWhile (fun c -> c <> '\r' || c <> '\n') |> List.skipWhile (fun c -> c = '\r' || c = '\n')
                    yield Paragraph(parseSpans' (prev |> List.rev |> implode) |> List.ofSeq)
                    yield! loop after []
                | false, _ ->
                    yield! loop rest ('\n'::'\r'::prev)
            | '\r' :: rest ->
                match rest |> List.takeWhile (fun c -> c <> '\r' || c <> '\n') |> List.forall System.Char.IsWhiteSpace, prev with
                | _ , x when x |> List.forall System.Char.IsWhiteSpace ->
                    yield! loop rest []
                | true, _ ->
                    let after = rest |> List.skipWhile (fun c -> c <> '\r' || c <> '\n') |> List.skipWhile (fun c -> c = '\r' || c = '\n')
                    yield Paragraph(parseSpans' (prev |> List.rev |> implode) |> List.ofSeq)
                    yield! loop after []
                | false, _ ->
                    yield! loop rest ('\r' :: prev)
            | '\n' :: rest ->
                match rest |> List.takeWhile (fun c -> c <> '\r' || c <> '\n') |> List.forall System.Char.IsWhiteSpace, prev with
                | _ , x when x |> List.forall System.Char.IsWhiteSpace ->
                    yield! loop rest []
                | true, _ ->
                    let after = rest |> List.skipWhile (fun c -> c <> '\r' || c <> '\n') |> List.skipWhile (fun c -> c = '\r' || c = '\n')
                    yield Paragraph(parseSpans' (prev |> List.rev |> implode) |> List.ofSeq)
                    yield! loop after []
                | false, _ ->
                    yield! loop rest ('\n' :: prev) 
            | x :: xs -> yield! loop xs (x::prev)
            | [] -> yield Paragraph(parseSpans' (prev |> List.rev |> implode) |> List.ofSeq)
        }
 
        (text |> explode, []) ||> loop
 
    let partitionWhile f =
        let rec loop f acc =
            function
            | x :: xs when f x -> loop f (x::acc) xs
            | xs -> (acc |> List.rev, xs)
    
        loop f []
 
    let (|AsCharList|) (str:string) =
        str |> List.ofSeq
 
    let (|LineSeparated|) input = 
        let isWhitespace = System.String.IsNullOrWhiteSpace
        match partitionWhile (isWhitespace >> not) input with
        | par, _::rest | par, ([] as rest) -> (par,rest)
 
    let (|LinePrefixed|) (prefix:string) (input:string list) =
        let prefixed, after = partitionWhile (fun (x:string) -> x.StartsWith prefix) input
        [ for line in prefixed -> line.Substring(prefix.Length)], after
 
    let (|LineFollowed|_|) c (input:string list) =
        //let p s = (System.String.IsNullOrWhiteSpace >> not) s && (String.forall ((=) c) s)
        let const' k _ = k
        let isWhitespace = System.String.IsNullOrWhiteSpace
        let p s =  (String.forall (fun ch -> ch = c || ch = ' ') s)
        match partitionWhile (isWhitespace >> not) input with
        | par, _::rest when (par |> List.length) > 1 && par |> List.tryLast |> Option.map p |> Option.fold (const' id) false -> Some (par |> List.take (List.length par - 1),rest)
        | _ -> None
    
    let (|Heading|_|) = function
        | AsCharList(StartsWith ['#'; ' '] heading)::rest ->
            Some (MarkdownBlock.Heading(1, heading |> implode |> parseSpans' |> List.ofSeq),rest)
        | AsCharList(StartsWith ['#';'#'; ' '] heading)::rest ->
            Some (MarkdownBlock.Heading(2, heading |> implode |> parseSpans' |> List.ofSeq),rest)
        | LineFollowed '=' (before,after) when before <> [] ->
            Some (MarkdownBlock.Heading(1, before |> String.concat " " |> parseSpans' |> List.ofSeq), after)
        | LineFollowed '-' (before,after) when before <> [] ->
            Some (MarkdownBlock.Heading(2, before |> String.concat " " |> parseSpans' |> List.ofSeq), after)
        | _ -> None
 
    let (|BlockQuote|_|) (input:string list) =
        let rec loop xs = 
            let prefixed, after = partitionWhile (fun (c:string) -> c.StartsWith "> ") xs
            if prefixed.Length > 1 then
                Some ([ for line in prefixed -> line.Substring(2)], after)
            elif prefixed.Length = 0 then None
            else
                match after with
                | LineSeparated (before', after') when (fst (partitionWhile (fun (c:string) -> c.StartsWith "> ") after') |> List.length) = 1 ->
                    match loop after' with
                    | Some (before'', after'') when before'' <> [] ->
                        let head = prefixed |> List.head |> (fun x -> x.Substring(2))
                        Some ([for line in List.Cons(head,before') |> List.append <| List.Cons("\r\n", before'') -> line], after'')
                    | None -> 
                        let head = prefixed |> List.head |> (fun x -> x.Substring(2))
                        Some ([for line in List.Cons(head,before') -> line], after')
                | LineSeparated (before, after)->
                    let head = prefixed |> List.head |> (fun x -> x.Substring(2))
                    Some ([for line in List.Cons(head, before) -> line], after)
 
        input |> loop
 
    let rec parseBlocks' text = seq {
        match text with
        | Heading (heading, rest) ->
            yield heading
            yield! parseBlocks' rest
        | BlockQuote (before, after) when before <> [] ->
            yield BlockQuote(before |> parseBlocks' |> List.ofSeq)
            yield! parseBlocks' after
        | LinePrefixed "    " (prefixed, rest) when prefixed <> [] ->
            yield CodeBlock(prefixed)
            yield! parseBlocks' rest
        | LineSeparated (before, after) when before <> [] ->
            yield Paragraph(before |> String.concat " " |> parseSpans' |> List.ofSeq)
            yield! parseBlocks' after
        | x :: xs -> 
            yield! parseBlocks' xs
        | _ -> ()
        }
 
    let parseMarkdown (text:string) =
        text.Split('\r','\n')
        |> List.ofArray
        |> parseBlocks'
