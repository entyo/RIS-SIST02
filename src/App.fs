module App

open Fable.React

let handleChange (ev: Browser.Types.Event) = printfn "%s" ev.Value

let vdom =
  div []
    [ textarea [ Props.DOMAttr.OnChange handleChange ] []
      button [] [ str "消去" ] ]

let render() = ReactDom.render (vdom, (Browser.Dom.document.getElementById "root"))

render()
