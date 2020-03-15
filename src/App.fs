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

// 参照のタイプ（最初のタグである必要があります）
let TY = "TY"
// 著者
let AU = "AU"
// 第一著者
let A1 = "A1"
// 副著者
let A2 = "A2"
// 三次著者
let A3 = "A3"
// 	補助著者
let A4 = "A4"
// 概要
let AB = "AB"
// 著者アドレス
let AD = "AD"
// 受入番号
let AN = "AN"
// アーカイブ内の場所
let AV = "AV"
let BT = "BT"
let PY = "PY"
let SP = "SP"
let EP = "EP"
let SN = "SN"
let TI = "TI"
let T1 = "T1"
let ER = "ER"
let VL = "VL"
let JO = "JO"

let ConvertedTag = [ TY; TI; T1; AU; PY; SP; EP; ER; VL; JO ]

let RISOrderLessTagParser =
  Parsimmon.str AU
  |> Parsimmon.orTry (Parsimmon.str PY)
  |> Parsimmon.orTry (Parsimmon.str SP)
  |> Parsimmon.orTry (Parsimmon.str EP)
  |> Parsimmon.orTry (Parsimmon.str SN)
  |> Parsimmon.orTry (Parsimmon.str T1)
  |> Parsimmon.orTry (Parsimmon.str VL)
  |> Parsimmon.orTry (Parsimmon.str JO)

let RISFieldParser tagParser =
  Parsimmon.seq5 tagParser (Parsimmon.optionalWhitespace) (Parsimmon.str "-") (Parsimmon.optionalWhitespace)
    (Parsimmon.regex ".*")
  |> Parsimmon.map (fun tuple ->
       { tag = _fst tuple
         value = fifth tuple })

let RISRecordParser =
  Parsimmon.seq3
    (RISFieldParser(Parsimmon.str TY)
     |> Parsimmon.skip (Parsimmon.str "\n"))
    (((Parsimmon.seperateBy (Parsimmon.str "\n") (RISFieldParser RISOrderLessTagParser)))
     |> Parsimmon.skip (Parsimmon.str "\n")) (RISFieldParser(Parsimmon.str ER))
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
          Props.Rows 8
          Props.Class "ris-input" ] []
      button [ Props.DOMAttr.OnClick(fun _ -> resetValue()) ] [ str "消去" ] ]

let vdom = div [] [ ofFunction inputRIS () [] ]

let render() = ReactDom.render (vdom, (Browser.Dom.document.getElementById "root"))

render()
