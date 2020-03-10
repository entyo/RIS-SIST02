module App

open Fable.React
open Fable.Core.JsInterop
open Fable.Parsimmon

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

let TYParser = Parsimmon.seq5 (Parsimmon.str TY) (Parsimmon.optionalWhitespace) (Parsimmon.str "-") (Parsimmon.optionalWhitespace) (Parsimmon.regex ".+")

let fifth  (_, _, _, _, e) = e

let useInputValue (initialValue: string) =
  let (value, setValue) = useState (initialValue)

  let onChange (e: Browser.Types.Event) =
    let value: string = e.target?value
    let result = TYParser.parse value
    if result.status then (fifth result.value) |> printfn "TY: %s" else printfn "Failed to parse"

    setValue (value)

  let resetValue() = setValue (System.String.Empty)

  value, onChange, resetValue

let inputRIS (_: obj) =
  let (value, onChange, resetValue) = useInputValue ""
  let placeholder = "TY  - CHAP\nAU  - Islam, Gazi\nPY  - 2014/07/29\nSP  - 1781\nEP  - 1783\nSN  - 978-1-4614-5584-4\nT1  - Social Identity Theory\nER  - "

  div []
    [ textarea
        [ Props.Value value
          Props.DOMAttr.OnChange onChange
          Props.Placeholder placeholder 
          Props.Rows 8
          Props.Class "ris-input"
        ] []
      button [ Props.DOMAttr.OnClick(fun _ -> resetValue()) ] [ str "消去" ] ]

let vdom = div [] [ ofFunction inputRIS () [] ]

let render() = ReactDom.render (vdom, (Browser.Dom.document.getElementById "root"))

render()
