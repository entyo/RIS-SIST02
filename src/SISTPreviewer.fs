module SISTPreviewer

open Fable.React
open CopyToClipboard

Fable.Core.JsInterop.importAll "./SIST02Previewer.css"

type SISTStr = option<string>

type Props =
  { sistStr: SISTStr }

let placeholder =
  "Plumbaum, Till.; Wu, Songxuan.; De Luca, Ernesto.; Albayrak, Sahin. User Modeling for the Social Semantic Web. CEUR Workshop Proceedings, vol.781, 2011/01/01."

let sistStrPreviewer (props: Props) =
  let copiedHooks = Hooks.useState false

  Hooks.useEffect ((fun () -> copiedHooks.update false), [| props.sistStr |])

  let previewer (fa: string option) =
    let sist =
      match fa with
      | Some s -> p [] [ str s ]
      | None -> p [ Props.ClassName "placeholder" ] [ str placeholder ]
    div [ Props.ClassName "preview-container" ] [ sist ]

  match props.sistStr with
  | Some sist ->
      div []
        [ sist
          |> Some
          |> previewer
          div [ Props.ClassName "button-row" ]
            [ copyToClipboard
                [ Text sist
                  OnCopy(fun () -> copiedHooks.update true) ]
                [ button [ Props.Disabled copiedHooks.current ] [ str (if copiedHooks.current then "コピーしました" else "クリップボードにコピー") ] ] ] ]
  | None -> None |> previewer
