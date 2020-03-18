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
  let sistHook = Hooks.useState<option<either<string, string>>> None

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

  let err =
    match sistHook.current with
    | Some fa ->
        match fa with
        | Right _ -> ""
        | Left err -> err
    | None -> ""

  let textAreaValue =
    match userInput with
    | Some txt -> txt
    | None -> ""

  let sistStr =
    match sistHook.current with
    | Some fa ->
        match fa with
        | Right v -> Some v
        | Left _ -> None
    | None -> None

  div []
    [ h1 [] [ str "RIS -> SIST02 Converter" ]
      div [ Props.ClassName "two-pain" ]
        [ div []
            [ textarea
                [ Props.Value textAreaValue
                  Props.DOMAttr.OnChange onUserInputChange
                  Props.Placeholder
                    "TY  - JOUR\nAU  - Plumbaum, Till\nAU  - Wu, Songxuan\nAU  - De Luca, Ernesto\nAU  - Albayrak, Sahin\nPY  - 2011/01/01\nSP  - \nT1  - User Modeling for the Social Semantic Web\nVL  - 781\nJO  - CEUR Workshop Proceedings\nER  - "
                  Props.Rows 15
                  Props.Class "ris-input" ] []
              div [ Props.Class "error-row" ] [ str err ]
              div [ Props.ClassName "button-row" ]
                [ button [ Props.DOMAttr.OnClick(fun _ -> resetInputValue()) ] [ str "消去" ] ] ]
          sistStrPreviewer ({ sistStr = sistStr }) ] ]


let vdom = div [] [ ofFunction Container () [] ]

let render() = ReactDom.render (vdom, (Browser.Dom.document.getElementById "root"))

render()
