#load "Markdown.fs"
#load "Parser.fs"
#load "HTMLFormat.fs"
#load "PostProcessing.fs"

open MarkdownWithFs
open Markdown
open Parser
open HTMLFormat
open PostProcessing

let sample = """
# Visual F#
 
> This is a blockquote with two paragraphs. Lorem ipsum dolor sit amet,
> consectetuer adipiscing elit. Aliquam hendrerit mi posuere lectus.
> Vestibulum enim wisi, viverra nec, fringilla in, laoreet vitae, risus.
> 
> Donec sit amet nisl. Aliquam semper ipsum sit amet velit. Suspendisse
> id sem consectetuer libero luctus adipiscing.
 
> This is a blockquote with two paragraphs. Lorem ipsum dolor sit amet,
consectetuer adipiscing elit. Aliquam hendrerit mi posuere lectus.
Vestibulum enim wisi, viverra nec, fringilla in, laoreet vitae, risus.
 
> Donec sit amet nisl. Aliquam semper ipsum sit amet velit. Suspendisse
id sem consectetuer libero luctus adipiscing.
 
> ## This is a header.
> 
> 1.   This is the first list item.
> 2.   This is the second list item.
> 
> Here's some example code:
> 
>     return shell_exec("echo $input | $markdown_script");
 
F# is a **programming language** that supports _functional_, as       
well as _object-oriented_ and _imperative_ programming styles.     
   
Hello world can be written as follows:  
                              
    printfn "Hello world!"
    printfn "Hello world! once again"        
          
For more information, see the [F# home page](http://fsharp.net) or 
read [Real-World Func tional Programming](http://manning.com/petricek) 
published by [Manning](http://manning.com).
"""

let rec generateSpanRefs (refs:ResizeArray<_>) = function
    | HyperLink(body,url) as span ->
        let id = sprintf "[%d]" (refs.Count + 1)
        refs.Add(id,url)
        [span; Literal (id)]
    | SpanNode(shape,children) ->
        let children = children |> List.collect (generateSpanRefs refs)
        [SpanNode(shape, children)]
    | span -> [span]

let rec generateBlockRefs refs = function
    | BlockNode(shape, children) ->
        let children = children |> List.collect (generateSpanRefs refs)
        BlockNode(shape, children)
    | block -> block

let refs = ResizeArray<_>()
let printable = sample |> parseMarkdown |> List.ofSeq |> List.map (generateBlockRefs refs)

let referencesHeading = [Heading(1, [Literal("References")])]
let referenceLinks = refs |> Seq.map(fun (id,url) -> 
    seq { 
        yield Literal(id)
        yield HyperLink([Literal(url)], url)
        yield HardLineBreak
    })

let referencesParagraph = referenceLinks |> Seq.concat |> List.ofSeq |> Paragraph
let references = referencesHeading @ [referencesParagraph]

let markdownWithReferences = printable @ references

markdownWithReferences |> exportToHTML