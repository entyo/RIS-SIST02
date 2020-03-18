module SISTPreviewer

open Fable.React
open Fable.Core.JsInterop
open CopyToClipboard

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
