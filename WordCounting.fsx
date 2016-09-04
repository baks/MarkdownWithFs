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

let rec countWordsInSpans = function
    | Literal text ->
        text.Split(Array.empty<char>, System.StringSplitOptions.RemoveEmptyEntries) |> Array.length
    | SpanNode(shape, children) ->
        children |> List.map countWordsInSpans |> List.sum
    | span -> 
        0

let rec countWordsInBlocks = function
    | BlockNode(shape,children) -> 
        children |> List.map countWordsInSpans |> List.sum
    | BlockQuoteNode(shape, children) ->
        children |> List.map countWordsInBlocks |> List.sum
    | block -> 
        0

let md = sample |> parseMarkdown
let words = sample |> parseMarkdown |> Seq.map countWordsInBlocks |> Seq.sum