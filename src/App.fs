module App

open Fable.React
open Fable.Core.JsInterop
open Fable.Parsimmon
open RIS

type SetState<'t> = 't -> unit

// https://github.com/nojaf/fable-react-hooks-sample/blob/master/src/Bindings.fs#L7
let useState<'t> (t: 't): 't * SetState<'t> = import "useState" "react"

let useEffect (effect : unit -> Fable.Core.U2<unit, unit -> unit>) (dependsOn : obj array) : unit = import "useEffect" "react"

let useInputValue (initialValue: string) =
  let (value, setValue) = useState (initialValue)

  let onChange (e: Browser.Types.Event) =
    let value: string = e.target?value   
    setValue (value)

  let resetValue() = setValue (System.String.Empty)

  value, onChange, resetValue

type Props = { sistStr: string }

let sistStrPreviewer (props: Props) =
  p [] [str props.sistStr]

let Container () =
  let (userInput, onUserInputChange, resetInputValue) = useInputValue ""
  let (sistStr, setSistStr) = useState ""
  let placeholder =
    "TY  - CHAP\nAU  - Islam, Gazi\nPY  - 2014/07/29\nSP  - 1781\nEP  - 1783\nSN  - 978-1-4614-5584-4\nT1  - Social Identity Theory\nER  - "
  
  // https://github.com/nojaf/fable-react-hooks-sample/blob/master/src/UseEffect.fs#L28
  useEffect (fun () ->
    let headerRecordsPerser = (HeaderParser |> RISHeaderFieldParser) |> Parsimmon.many
    let awesomeParser = (Parsimmon.seq2 headerRecordsPerser RISRecordParser) |> Parsimmon.map snd
    let result = awesomeParser.parse userInput
    if result.status then
      let grouped =
        (result.value
         |> Seq.filter (fun r ->
              (r.value = "" || System.Text.RegularExpressions.Regex.IsMatch(r.value, @"^\s+$")) |> not)
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
      let sist =
        [ [ authorPart; titlePart ]
          |> List.filter Option.isSome
          |> List.map (fun fa -> fa.Value)
          [ bibPart ] ]
        |> List.concat
        |> String.concat ". "

      setSistStr sist |> Fable.Core.U2.Case1
    else setSistStr "Failed to parse" |> Fable.Core.U2.Case1
  ) [| userInput |]

  div []
    [ textarea
        [ Props.Value userInput
          Props.DOMAttr.OnChange onUserInputChange
          Props.Placeholder placeholder
          Props.Rows 15
          Props.Class "ris-input" ] []
      button [ Props.DOMAttr.OnClick(fun _ -> resetInputValue()) ] [ str "消去" ]
      sistStrPreviewer({ sistStr = sistStr }) ]


let vdom = div [] [ ofFunction Container () [] ]

let render() = ReactDom.render (vdom, (Browser.Dom.document.getElementById "root"))

render()
