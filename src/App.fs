module App

open Fable.React
open Fable.Core.JsInterop
open Fable.Parsimmon
open RIS
open Either
open SISTPreviewer
open RISToSIST02

importAll "./App.css"

let useInputValue (initialValue: option<string>) =
  let stateHook = Hooks.useState (initialValue)

  let onChange (e: Browser.Types.Event) =
    let value: string = e.target?value
    stateHook.update (value |> Some)

  let resetValue() = stateHook.update None

  stateHook.current, onChange, resetValue

// https://github.com/nkbt/react-copy-to-clipboard/blob/master/src/Component.js#L7
let Container() =
  let (userInput, onUserInputChange, resetInputValue) = useInputValue None
  let sistHook = Hooks.useState<SISTStr> None

  let effectFn =
    fun () ->
      match userInput with
      | Some input ->
          let headerRecordsPerser = (HeaderParser |> RISHeaderFieldParser) |> Parsimmon.many
          let awesomeParser = (Parsimmon.seq2 headerRecordsPerser RISRecordParser) |> Parsimmon.map snd
          let result = awesomeParser.parse input
          if result.status then
            sistHook.update
              (result.value
               |> sistStrFromRISFields
               |> Right
               |> Some)
          else
            sistHook.update (Left "不正な入力です" |> Some)
      | None -> sistHook.update None

  Hooks.useEffect (effectFn, [| userInput |])

  let textAreaValue =
    match userInput with
    | Some txt -> txt
    | None -> ""

  div []
    [ textarea
        [ Props.Value textAreaValue
          Props.DOMAttr.OnChange onUserInputChange
          Props.Placeholder
            "TY  - CHAP\nAU  - Islam, Gazi\nPY  - 2014/07/29\nSP  - 1781\nEP  - 1783\nSN  - 978-1-4614-5584-4\nT1  - Social Identity Theory\nER  - "
          Props.Rows 15
          Props.Class "ris-input" ] []
      button [ Props.DOMAttr.OnClick(fun _ -> resetInputValue()) ] [ str "消去" ]
      sistStrPreviewer ({ sistStr = sistHook.current }) ]


let vdom = div [] [ ofFunction Container () [] ]

let render() = ReactDom.render (vdom, (Browser.Dom.document.getElementById "root"))

render()
