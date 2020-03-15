module App

open Fable.React
open Fable.Core.JsInterop
open Fable.Parsimmon
open RIS

type SetState<'t> = 't -> unit

let useState<'t> (t: 't): 't * SetState<'t> = import "useState" "react"

let useInputValue (initialValue: string) =
  let (value, setValue) = useState (initialValue)

  let onChange (e: Browser.Types.Event) =
    let value: string = e.target?value
    let headerRecordsPerser = (HeaderParser |> RISHeaderFieldParser) |> Parsimmon.many
    let awesomeParser = (Parsimmon.seq2 headerRecordsPerser RISRecordParser) |> Parsimmon.map snd
    let result = awesomeParser.parse value
    if result.status then
      let grouped = (result.value |> Seq.groupBy (fun v -> 
        match v.tag with
        | "T1" | "TI" -> "title"
        | "AU" -> "author"
        | "PY" -> "year"
        | "JO" -> "journal"
        | "VL" -> "volume"
        | "IS" -> "issue"
        | "SP" -> "start"
        | "EP" -> "end"
        | _ -> ""
      ))
      grouped |> Seq.iter (fun g -> 
        printfn "# %s" (fst g)
        (snd g) |> Seq.iter (fun v -> printfn "%s" v.value)
      )
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
