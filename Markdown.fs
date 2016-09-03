namespace MarkdownWithFs

module Markdown =
 
    type MarkdownDocument = MarkdownBlock list
    and MarkdownBlock =
        | Heading of int * MarkdownSpans
        | Paragraph of MarkdownSpans
        | CodeBlock of string list
        | BlockQuote of MarkdownDocument
    and MarkdownSpans = MarkdownSpan list
    and MarkdownSpan =
        | Literal of string
        | InlineCode of string
        | Strong of MarkdownSpans
        | Emphasis of MarkdownSpans
        | HyperLink of MarkdownSpans * string
        | HardLineBreak