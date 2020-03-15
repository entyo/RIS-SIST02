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

      let journalPart =
        grouped
        |> Seq.tryFind (fun g -> (fst g) = "journal")
        |> Option.map snd
        |> Option.map (Seq.map (fun r -> r.value))
        |> Option.bind (Seq.tryHead)
      // year, volume, issue, start, end
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

      let bibPart =
        sprintf "%s, %s(%s), p.%s-%s." (year |> Option.defaultValue "YYYY") (volume |> Option.defaultValue "V")
          (issue |> Option.defaultValue "I") (startp |> Option.defaultValue "S") (endp |> Option.defaultValue "E")
      // https://jipsti.jst.go.jp/sist/handbook/sist02sup/sist02sup.htm#5.1.1
      let result =
        String.concat ". "
          [ (authorPart |> Option.defaultValue "A")
            (titlePart |> Option.defaultValue "T")
            (journalPart |> Option.defaultValue "J")
            bibPart ]
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
