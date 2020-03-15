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
      let grouped =
        (result.value
         |> Seq.filter (fun r -> (r.value = "" || System.Text.RegularExpressions.Regex.IsMatch(r.value, "\s+")) |> not)
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
        |> Seq.find (fun g -> (fst g) = "author")
        |> snd
        |> Seq.map (fun r -> r.value + ".")
        |> String.concat "; "

      let titlePart =
        grouped
        |> Seq.find (fun g -> (fst g) = "title")
        |> snd
        |> Seq.map (fun r -> r.value)
        |> String.concat ": "

      printfn "titlePart: %s" titlePart
      let journalPart =
        grouped
        |> Seq.find (fun g -> (fst g) = "journal")
        |> snd
        |> Seq.map (fun r -> r.value)
        |> Seq.head
      // year, volume, issue, start, end
      let year =
        grouped
        |> Seq.find (fun g -> (fst g) = "year")
        |> snd
        |> Seq.map (fun r -> r.value)
        |> Seq.head
      let volume =
        grouped
        |> Seq.find (fun g -> (fst g) = "volume")
        |> snd
        |> Seq.map (fun r -> r.value)
        |> Seq.head
      let issue =
        grouped
        |> Seq.find (fun g -> (fst g) = "issue")
        |> snd
        |> Seq.map (fun r -> r.value)
        |> Seq.head
      let startp =
        grouped
        |> Seq.find (fun g -> (fst g) = "start")
        |> snd
        |> Seq.map (fun r -> r.value)
        |> Seq.head
      let endp =
        grouped
        |> Seq.find (fun g -> (fst g) = "end")
        |> snd
        |> Seq.map (fun r -> r.value)
        |> Seq.head

      let bibPart = sprintf "%s, %s(%s), p.%s-%s." year volume issue startp endp

      let result = sprintf "%s %s. %s. %s" authorPart titlePart journalPart bibPart
      printfn "%s" result
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
