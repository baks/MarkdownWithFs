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
## Nested header
 
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

# Second header
## Nested in second header
F# is a **programming language** that supports _functional_, as       
well as _object-oriented_ and _imperative_ programming styles.     
   
Hello world can be written as follows:  
                              
    printfn "Hello world!"
    printfn "Hello world! once again"        
          
For more information, see the [F# home page](http://fsharp.net) or 
read [Real-World Func tional Programming](http://manning.com/petricek) 
published by [Manning](http://manning.com).
"""

let levels = 
    [ 1,0;
      2,0;
      3,0;
      4,0; ]
    |> Map.ofList

let incrementLevel idx map = 
    map
    |> Map.map (fun key el -> if key = idx then el+1 else el)
    |> Map.map (fun key el -> if key > idx then 0 else el)

let getHeaderNumber idx map = 
    map
    |> Map.filter (fun key el -> key <= idx)
    |> Map.toSeq
    |> Seq.map (fun (key,el) -> sprintf "%i" el)
    |> String.concat "."
    |> fun x -> 
        if idx = 0 then
            x + ". "
        else
            x + " "

let rec numberHeadings lvls = function
    | MarkdownBlock.Heading(size, body) as h ->
        let levels' = incrementLevel size lvls
        let number = levels'.[size]
        (levels',Heading(size, Literal(getHeaderNumber size levels') :: body))
    | block -> 
        (lvls,block)

let numberedDoc = sample |> parseMarkdown |> Seq.fold (fun (s,b) el -> 
    let (s', newEl) = numberHeadings s el
    (s', newEl::b)) (levels, []) |> snd |> Seq.rev

numberedDoc |> List.ofSeq |> exportToHTML