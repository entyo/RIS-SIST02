module SISTPreviewer

open Fable.React
open CopyToClipboard
open Fable.FontAwesome

Fable.Core.JsInterop.importAll "./SIST02Previewer.css"

type SISTStr = option<option<string>>

type Props =
  { sistStr: SISTStr }

let placeholder =
  "Gurrin, Cathal.; Joho, Hideo.; Hopfgartner, Frank.; Zhou, Liting.; Albatal, Rami. NTCIR Lifelog: The First Test Collection for Lifelog Research. 2016/07/07, p.705-708."

let sistStrPreviewer (props: Props) =
  let copiedHooks = Hooks.useState false

  Hooks.useEffect ((fun () -> copiedHooks.update false), [| props.sistStr |])

  let previewer (sist: SISTStr) =
    let sist =
      match sist with
      | Some fa ->
          match fa with
          | Some s -> p [] [ str s ]
          | None -> p [ Props.ClassName "no-select" ] [ str "RIS形式のCitationを入力してください" ]
      | None -> p [ Props.ClassName "placeholder no-select" ] [ str placeholder ]
    div [ Props.ClassName "preview-container" ] [ sist ]

  let (sist, copiable) =
    match props.sistStr with
    | Some fa ->
        match fa with
        | Some str -> (str, true)
        | None -> ("", false)
    | None -> ("", false)

  printfn "copiedHooks.current %b, copiable %b" copiedHooks.current copiable

  div [ Props.ClassName "container" ]
    [ props.sistStr |> previewer
      div [ Props.ClassName "button-row" ]
        [ copyToClipboard
            [ Text sist
              OnCopy(fun () -> copiedHooks.update true) ]
            [ button [ (copiedHooks.current || copiable |> not) |> Props.Disabled ]
                (if copiedHooks.current then
                  [ Fa.span [ Fa.Solid.Check; Fa.FixedWidth ] []
                    str "コピーしました" ]
                 else
                   [ Fa.span [ Fa.Solid.Clone; Fa.FixedWidth ] []
                     str "クリップボードにコピー" ]) ] ] ]
