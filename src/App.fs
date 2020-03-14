module App

open Fable.React
open Fable.Core.JsInterop
open Fable.Parsimmon

let _fst (a, _, _, _, _) = a
let fifth (_, _, _, _, e) = e

type RISField =
  { tag: string
    value: string }

type SetState<'t> = 't -> unit

let useState<'t> (t: 't): 't * SetState<'t> = import "useState" "react"

let TY = "TY"
let AU = "AU"
let PY = "PY"
let SP = "SP"
let EP = "EP"
let SN = "SN"
let T1 = "T1"
let ER = "ER"

let RISTagParser =
  (Parsimmon.str TY)
  |> Parsimmon.orTry (Parsimmon.str AU)
  |> Parsimmon.orTry (Parsimmon.str PY)
  |> Parsimmon.orTry (Parsimmon.str SP)
  |> Parsimmon.orTry (Parsimmon.str EP)
  |> Parsimmon.orTry (Parsimmon.str SN)
  |> Parsimmon.orTry (Parsimmon.str T1)
  |> Parsimmon.orTry (Parsimmon.str ER)

let RISFieldParser =
  Parsimmon.seq5 RISTagParser (Parsimmon.optionalWhitespace) (Parsimmon.str "-") (Parsimmon.optionalWhitespace)
    (Parsimmon.regex ".*")
  |> Parsimmon.map (fun tuple ->
       { tag = _fst tuple
         value = fifth tuple })

let RISRecordParser =
  Parsimmon.seperateByAtLeastOne (Parsimmon.str "\n") RISFieldParser

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
          Props.Rows 8
          Props.Class "ris-input" ] []
      button [ Props.DOMAttr.OnClick(fun _ -> resetValue()) ] [ str "消去" ] ]

let vdom = div [] [ ofFunction inputRIS () [] ]

let render() = ReactDom.render (vdom, (Browser.Dom.document.getElementById "root"))

render()
