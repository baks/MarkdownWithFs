namespace MarkdownWithFs

open Markdown

module PostProcessing =

    let (|SpanNode|_|) span =
        match span with
        | Strong spans | Emphasis spans | HyperLink (spans, _) ->
            Some(box span, spans)
        | _ -> None
        
    let SpanNode (span:obj, children) =
        match unbox span with
        | Strong _ -> Strong children
        | Emphasis _ -> Emphasis children
        | HyperLink (_, url) -> HyperLink(children, url)
        | _ -> invalidArg "" "Incorrect MarkdownSpan"
        
    let (|BlockNode|_|) block =
        match block with
        | Heading(_, spans) | Paragraph(spans) ->
            Some(box block, spans)
        | _ -> None
        
    let BlockNode (block:obj, children) =
        match unbox block with
        | Heading(size, _) -> Heading(size, children)
        | Paragraph _ -> Paragraph children
        | _ -> invalidArg "" "Incorrect MarkdownBlock" 

    let (|BlockQuoteNode|_|) blockquote =
        match blockquote with
        | BlockQuote md ->
            Some(box blockquote, md)
        | _ -> None

    let BlockQuoteNode (blockquote:obj, children) =
        match unbox blockquote with
        | BlockQuote _ ->
            BlockQuote children
        | _ -> invalidArg "" "Incorrect BlockQuote block"