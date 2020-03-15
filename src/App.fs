module App

open Fable.React
open Fable.Core.JsInterop
open Fable.Parsimmon
open RIS

type RISField =
  { tag: string
    value: string }

type SetState<'t> = 't -> unit

let useState<'t> (t: 't): 't * SetState<'t> = import "useState" "react"

let RISOrderLessTagParser =
  OrderLessTags
  |> List.map Parsimmon.str
  |> Parsimmon.choose

let RISFieldParser tagParser =
  Parsimmon.seq2
    (tagParser
    |> Parsimmon.skip (Parsimmon.optionalWhitespace)
    |> Parsimmon.skip (Parsimmon.str "-"))
    (Parsimmon.regex(".*").orTry(Parsimmon.optionalWhitespace))
  |> Parsimmon.skip ((Parsimmon.str "\n").orTry(Parsimmon.endOfFile))
  |> Parsimmon.map (fun tuple ->
       { tag = fst tuple
         value = snd tuple })

let RISRecordParser =
  Parsimmon.seq3 (RISFieldParser(Parsimmon.str TY)) (((Parsimmon.many (RISFieldParser RISOrderLessTagParser))))
    (RISFieldParser(Parsimmon.str ER))
  |> Parsimmon.map (fun (ty, orderless, er) ->
       Seq.concat
         [ [ ty ]
           orderless |> Seq.toList
           [ er ] ])

let useInputValue (initialValue: string) =
  let (value, setValue) = useState (initialValue)

  let onChange (e: Browser.Types.Event) =
    let value: string = e.target?value
    let result = RISRecordParser.parse value
    if result.status
    then result.value |> Seq.iter (fun v -> printfn "%s: %s" (v.tag) (v.value))
    else printfn "Failed to parse"

    setValue (value)

  let resetValue() = setValue (System.String.Empty)

  value, onChange, resetValue

let inputRIS (_: obj) =
  let (value, onChange, resetValue) = useInputValue ""
  let placeholder =
    "TY  - CHAP\nAU  - Islam, Gazi\nPY  - 2014/07/29\nSP  - 1781\nEP  - 1783\nSN  - 978-1-4614-5584-4\nT1  - Social Identity Theory\nER  - "

  div []
    [ textarea
        [ Props.Value value
          Props.DOMAttr.OnChange onChange
          Props.Placeholder placeholder
          Props.Rows 15
          Props.Class "ris-input" ] []
      button [ Props.DOMAttr.OnClick(fun _ -> resetValue()) ] [ str "消去" ] ]

let vdom = div [] [ ofFunction inputRIS () [] ]

let render() = ReactDom.render (vdom, (Browser.Dom.document.getElementById "root"))

render()
