module App

open Elmish
open Elmish.React
open Feliz
open Feliz.Router


// APP

type Page =
    | Home
    | Overview
    | Building
    | Research
    | Shipyard
    | Defense
    | Galaxy

type State = { CurrentPage: Page }

type Command =
    | SetUsernameInput of string
    | SwitchPage of Page

let init() = { CurrentPage = Home }


let update (command: Command) (state: State) = 
    match command with
    | SwitchPage nextPage -> { state with CurrentPage = nextPage }

let MenuItem (text: string, page: Page,  dispatch) = Html.button [
    prop.text text
    prop.onClick (fun _ -> dispatch (SwitchPage page))
]

let render (state: State) (dispatch: Command -> unit) =
    match state.CurrentPage with
    | Home -> 
        Html.div [
            prop.className "wrapper"
            prop.children [
                Html.div [
                    prop.className "wrapper-login"
                    prop.children [
                        Html.h1 "OGame"
                        Html.h2 "Clone with F#"
                        Html.input [
                            prop.placeholder "Username"
                            //prop.onChange (fun _ -> SetUsernameInput >> dispacth)
                        ]
                        Html.button [
                            prop.className "btn btn-primary"
                            prop.onClick (fun _ -> dispatch (SwitchPage Page.Overview))
                            prop.text "Start game"
                        ]
                    ]
                ]
            ]
        ]
    | Overview ->
        Html.div [
            Html.h1 "Overview"
            Html.div [
                MenuItem ("Building", Page.Building, dispatch)
                MenuItem ("Research", Page.Research, dispatch)
                MenuItem ("Shipyard", Page.Shipyard, dispatch)
                MenuItem ("Defense", Page.Defense, dispatch)
                MenuItem ("Galaxy", Page.Galaxy, dispatch)
            ]

            Html.div [
                Html.div [
                ]
                Html.span "1 field"
                Html.span "[1:0:1] Location"
                Html.span "-21C to 19C - Temperature"
            ]
        ]
    | Building -> Html.h1 "Building"
    | Research -> Html.h1 "Research"
    | Shipyard -> Html.h1 "Shipyard"
    | Defense -> Html.h1 "Defense"
    | Galaxy -> Html.h1 "Galaxy"


Program.mkSimple init update render
|> Program.withReactSynchronous "elmish-app"
|> Program.run