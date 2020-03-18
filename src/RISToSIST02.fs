module RISToSIST02

open RIS

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
