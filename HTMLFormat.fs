namespace MarkdownWithFs

open Markdown
open System.Text

module HTMLFormat =

    let appendLine s (sb:StringBuilder) = sb.AppendLine s
 
    let writeTag (builder:StringBuilder) tag (body:StringBuilder -> StringBuilder) =
        builder.Append(sprintf "<%s>" tag) 
        |> body 
        |> appendLine (sprintf "</%s>" tag)
 
    let writeTag' (builder:StringBuilder) tag attr (body:StringBuilder -> StringBuilder) =
        builder.Append(sprintf "<%s" tag) 
        |> fun x -> attr |> Seq.fold (fun s (k,v) -> x.Append(sprintf " %s=\"%s\"" k v)) x
        |> fun x -> x.Append(">")
        |> body 
        |> appendLine (sprintf "</%s>" tag)
 
    let markdownSpansToHTML (spans:MarkdownSpans) (builder) =
 
        let rec loop xs (sb:StringBuilder) =
            match xs with
            | Literal s :: xs -> 
                sb.Append(s) |> loop xs
            | InlineCode s :: xs -> 
                writeTag sb "code" (appendLine s) |> loop xs
            | Strong s :: xs -> 
                writeTag sb "strong" (loop s) |> loop xs
            | Emphasis s :: xs -> 
                writeTag sb "em" (loop s) |> loop xs
            | HyperLink(s,link) :: xs -> 
                writeTag' sb "a" [("href",link)] (loop s) |> loop xs
            | HardLineBreak :: xs -> 
                writeTag sb "br" (id) |> loop xs
            | [] -> sb
 
        loop spans builder
 
    let markdownToHTML (md:MarkdownDocument) (builder) =
            
        let rec loop xs sb =
            match xs with
            | Heading(v, spans) :: xs -> 
                writeTag sb (sprintf "h%i" v) (markdownSpansToHTML spans) |> loop xs
            | Paragraph spans :: xs -> 
                writeTag sb "p" (markdownSpansToHTML spans) |> loop xs
            | CodeBlock code :: xs -> 
                writeTag sb "pre" (fun strBuilder -> writeTag strBuilder "code" (fun x -> code |> List.fold (fun s el -> s.AppendLine(el)) x)) |> loop xs
            | BlockQuote md :: xs -> 
                writeTag sb "blockquote" (loop md) |> loop xs
            | [] -> sb
 
        loop md builder
 
    let exportToHTML (md:MarkdownDocument) =
        markdownToHTML md (new StringBuilder()) |> (fun x -> x.ToString())

