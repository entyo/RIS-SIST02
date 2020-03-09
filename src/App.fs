module App

open Fable.React

let render() =
    ReactDom.render(
        p [] [str "fuck"],
        (Browser.Dom.document.getElementById "root"))

render()