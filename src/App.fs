module App

open Fable.React
open Fable.Core.JsInterop
open Fable.Parsimmon
open RIS
open Either

type CopyToClipboardProps =
  | Text of string
  | Children of ReactElement
  | OnCopy of (unit -> unit)
  | Options of obj

let inline copyToClipboard (props: CopyToClipboardProps list) (elems: ReactElement list): ReactElement =
  // https://github.com/nkbt/react-copy-to-clipboard#usage
  ofImport "CopyToClipboard" "react-copy-to-clipboard" (keyValueList Fable.Core.CaseRules.LowerFirst props) elems

let useInputValue (initialValue: option<string>) =
  let stateHook = Hooks.useState (initialValue)

  let onChange (e: Browser.Types.Event) =
    let value: string = e.target?value
    stateHook.update (value |> Some)

  let resetValue() = stateHook.update None

  stateHook.current, onChange, resetValue

type SISTStr = option<either<string, string>>

type Props =
  { sistStr: SISTStr }

let sistStrPreviewer (props: Props) =
  let copiedHooks = Hooks.useState false

  Hooks.useEffect ((fun () -> copiedHooks.update false), [| props.sistStr |])

  let result =
    match props.sistStr with
    | Some parseResult -> parseResult
    | None -> Left "RIS形式のデータを入力してください。"

  match result with
  | Right sist ->
      div []
        [ p [] [ str sist ]
          copyToClipboard
            [ Text sist
              OnCopy(fun () -> copiedHooks.update true) ] [ button [] [ str "クリップボードにコピー" ] ]
          span [] [ str (if copiedHooks.current then "コピーしました" else "") ] ]
  | Left err -> p [ Props.ClassName "error" ] [ str err ]

let sistStrFromRISFields (fields: seq<RISField>) =
  let grouped =
    (fields
     |> Seq.filter (fun r -> (r.value = "" || System.Text.RegularExpressions.Regex.IsMatch(r.value, @"^\s+$")) |> not)
     |> Seq.groupBy (fun v ->
          match v.tag with
          | "T1"
          | "TI" -> "title"
          | "AU" -> "author"
          | "PY" -> "year"
          | "JO" -> "journal"
          | "VL" -> "volume"
          | "IS" -> "issue"
          | "SP" -> "start"
          | "EP" -> "end"
          | _ -> "others"))

  let authorPart =
    grouped
    |> Seq.tryFind (fun g -> (fst g) = "author")
    |> Option.map snd
    |> Option.map (Seq.map (fun r -> r.value))
    |> Option.map (String.concat ".; ")

  let titlePart =
    grouped
    |> Seq.tryFind (fun g -> (fst g) = "title")
    |> Option.map snd
    |> Option.map (Seq.map (fun r -> r.value))
    |> Option.map (String.concat ": ")

  let journal =
    grouped
    |> Seq.tryFind (fun g -> (fst g) = "journal")
    |> Option.map snd
    |> Option.map (Seq.map (fun r -> r.value))
    |> Option.bind (Seq.tryHead)

  let year =
    grouped
    |> Seq.tryFind (fun g -> (fst g) = "year")
    |> Option.map snd
    |> Option.map (Seq.map (fun r -> r.value))
    |> Option.bind (Seq.tryHead)

  let volume =
    grouped
    |> Seq.tryFind (fun g -> (fst g) = "volume")
    |> Option.map snd
    |> Option.map (Seq.map (fun r -> r.value))
    |> Option.bind (Seq.tryHead)

  let issue =
    grouped
    |> Seq.tryFind (fun g -> (fst g) = "issue")
    |> Option.map snd
    |> Option.map (Seq.map (fun r -> r.value))
    |> Option.bind (Seq.tryHead)

  let startp =
    grouped
    |> Seq.tryFind (fun g -> (fst g) = "start")
    |> Option.map snd
    |> Option.map (Seq.map (fun r -> r.value))
    |> Option.bind (Seq.tryHead)

  let endp =
    grouped
    |> Seq.tryFind (fun g -> (fst g) = "end")
    |> Option.map snd
    |> Option.map (Seq.map (fun r -> r.value))
    |> Option.bind (Seq.tryHead)

  let volumeAndIssue =
    volume
    |> Option.map (fun v ->
         issue
         |> Option.map (fun iss -> sprintf "%s(%s)" v iss)
         |> Option.defaultValue (sprintf "vol.%s" v))

  let pageRange =
    startp
    |> Option.map (fun sp ->
         endp
         |> Option.map (fun ep -> sprintf "p.%s-%s" sp ep)
         |> Option.defaultValue (sprintf "p.%s-" sp))

  let bibPart =
    ([ journal; volumeAndIssue; year; pageRange ]
     |> List.filter Option.isSome
     |> List.map (fun fa -> fa.Value)
     |> String.concat ", ")
    + "."

  // https://jipsti.jst.go.jp/sist/handbook/sist02sup/sist02sup.htm#5.1.1
  [ [ authorPart; titlePart ]
    |> List.filter Option.isSome
    |> List.map (fun fa -> fa.Value)
    [ bibPart ] ]
  |> List.concat
  |> String.concat ". "

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
