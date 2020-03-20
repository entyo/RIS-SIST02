module App

open Fable.React
open Fable.Core.JsInterop
open Fable.Parsimmon
open RIS
open SISTPreviewer
open RISToSIST02
open Fable.FileInput.React

importAll "./App.css"

type UserInput =
  | File of Fable.FileInput.FileInfo<string>
  | KeyEvent of Browser.Types.Event

let useInputValue (initialValue: option<string>) =
  let stateHook = Hooks.useState (initialValue)

  let onChange (input: UserInput) =
    let value =
      match input with
      | KeyEvent e -> e.target?value
      | File file -> file.Data
    stateHook.update (if value = "" then None else value |> Some)

  let resetValue() = stateHook.update None

  stateHook.current, onChange, resetValue

let fileInputId = "fileinput"

// Reset <input type="file">
[<Fable.Core.Emit("const elem = document.getElementById($0); if (elem && elem.value) { elem.value = '' }")>]
let resetFileInput (id: string) : unit = Fable.Core.Util.jsNative

// https://github.com/nkbt/react-copy-to-clipboard/blob/master/src/Component.js#L7
let Container() =
  let (userInput, onUserInputChange, resetInputValue) = useInputValue None
  let sistHook = Hooks.useState<option<option<string>>> None

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
               |> Some
               |> Some)
          else
            sistHook.update (None |> Some)
      | None ->
        sistHook.update None
        resetFileInput fileInputId

  Hooks.useEffect (effectFn, [| userInput |])

  let textAreaValue =
    match userInput with
    | Some txt -> txt
    | None -> ""

  div []
    [ h1 [] [ str "RIS -> SIST02 Converter" ]
      div []
        [ div [ Props.ClassName "two-pain" ]
            [ div []
                [ textarea
                    [ Props.Value textAreaValue
                      Props.DOMAttr.OnChange(KeyEvent >> onUserInputChange)
                      Props.Placeholder
                        "TY  - JOUR\nAU  - Plumbaum, Till\nAU  - Wu, Songxuan\nAU  - De Luca, Ernesto\nAU  - Albayrak, Sahin\nPY  - 2011/01/01\nSP  - \nT1  - User Modeling for the Social Semantic Web\nVL  - 781\nJO  - CEUR Workshop Proceedings\nER  - "
                      Props.Rows 15
                      Props.Class "ris-input" ] []
                  div [ Props.ClassName "button-row" ]
                    [ singleFileInput [ OnTextReceived(File >> onUserInputChange); Props.Id fileInputId ]
                      button
                        [ Props.DOMAttr.OnClick(fun _ -> resetInputValue())
                          Props.Disabled(sistHook.current |> Option.isNone) ] [ str "消去" ] ] ]
              sistStrPreviewer ({ sistStr = sistHook.current }) ] ] ]


let vdom = div [] [ ofFunction Container () [] ]

let render() = ReactDom.render (vdom, (Browser.Dom.document.getElementById "root"))

render()
