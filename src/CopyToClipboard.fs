module CopyToClipboard

open Fable.React
open Fable.Core.JsInterop

type CopyToClipboardProps =
  | Text of string
  | Children of ReactElement
  | OnCopy of (unit -> unit)
  | Options of obj

let inline copyToClipboard (props: CopyToClipboardProps list) (elems: ReactElement list): ReactElement =
  // https://github.com/nkbt/react-copy-to-clipboard#usage
  ofImport "CopyToClipboard" "react-copy-to-clipboard" (keyValueList Fable.Core.CaseRules.LowerFirst props) elems
