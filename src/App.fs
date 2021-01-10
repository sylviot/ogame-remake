module App

open Elmish
open Elmish.React
open Feliz
open Feliz.Router

open Fable.Core 
// APP

type Page =
    | Home
    | Overview
    | Building
    | Research
    | Shipyard
    | Defense
    | Galaxy

type Build = { Name: string; Description: string; Level: int; }
type Builds =
    | MetalMine
    | CrystalMine
    | DeuteriumMine
    | Research
    | Shipyard

type State = { CurrentPage: Page; 
    Metal: int; Crystal: int; Deuterium: int;
    MetalMineLevel: int; CrystalMineLevel: int; DeuteriumMineLevel: int;
    ResearchLevel: int; ShipyardLevel: int;
    Building: Builds option}

type Command =
    | Tick
    | SwitchPage of Page
    | BuildUpgrade of Builds
    | BuildUpgraded of Builds

let init() = { 
    CurrentPage = Home;
    Metal = 0;
    Crystal = 0;
    Deuterium = 0;
    MetalMineLevel =  1;
    CrystalMineLevel = 1;
    DeuteriumMineLevel = 1;
    ResearchLevel = 0;
    ShipyardLevel = 0;
    Building = None; }, Cmd.none

let update (command: Command) (state: State) = 
    let buildTick (dispatch: Command -> unit) : unit =
        let tick = async {
            do! Async.Sleep 1000
            dispatch Tick
        }
        Async.StartImmediate tick

    match command with
    | Tick -> {state with 
        Metal = state.Metal + 100 + state.MetalMineLevel * 10;
        Crystal = state.Crystal + 100 + state.CrystalMineLevel * 10; }, Cmd.ofSub buildTick
    | SwitchPage nextPage ->
        match nextPage with
        | Overview -> { state with CurrentPage = nextPage }, Cmd.ofSub buildTick
        | _ -> { state with CurrentPage = nextPage }, Cmd.none
    | BuildUpgraded build ->
        match build with
        | MetalMine -> { state with MetalMineLevel = state.MetalMineLevel + 1; Building = None }, Cmd.none
        | CrystalMine -> { state with CrystalMineLevel = state.CrystalMineLevel + 1; Building = None }, Cmd.none
        | DeuteriumMine -> { state with DeuteriumMineLevel = state.DeuteriumMineLevel + 1; Building = None }, Cmd.none
        | Research -> { state with ResearchLevel = state.ResearchLevel + 1; Building = None }, Cmd.none
        | Shipyard -> { state with ShipyardLevel = state.ShipyardLevel + 1; Building = None }, Cmd.none
    | BuildUpgrade build -> 
        match state.Building with
        | None ->
            let buildingWithTime (dispatch: Command -> unit) : unit =
                let progress = async {
                    do! Async.Sleep 1000
                    dispatch  (BuildUpgraded build)
                }
                Async.StartImmediate progress
            {state with Building = Some build}, Cmd.ofSub buildingWithTime
        | Some -> state, Cmd.none

let MenuItem (text: string, page: Page,  dispatch) = Html.button [
    prop.text text
    prop.onClick (fun _ -> dispatch (SwitchPage page))
]
let Menu (state: State, dispatch) = Html.div [
    Html.span (sprintf "Metal %d" state.Metal)
    Html.span (sprintf "Crystal %d" state.Crystal)
    Html.span (sprintf "Deuterium %d" state.Deuterium)
    MenuItem ("Building", Page.Building, dispatch)
    MenuItem ("Research", Page.Research, dispatch)
    MenuItem ("Shipyard", Page.Shipyard, dispatch)
    MenuItem ("Defense", Page.Defense, dispatch)
    MenuItem ("Galaxy", Page.Galaxy, dispatch)
]

// Link: https://dev.to/semuserable/starting-with-fable-f-kbi
//type FrameRequestCallback = Func<float,unit>
// interface
//type Window =
//    // function description
//    abstract alert: ?message: string -> unit
//    abstract requestAnimationFrame:  callback:FrameRequestCallback -> float

// wiring-up JavaScript and F# with [<Global>] and jsNative
//let [<Global>] window: Window = jsNative

// client calls
//window.alert ("Global Fable window.alert")
//JS.console.log (Browser.document)

let render (state: State) (dispatch: Command -> unit) =
    match state.CurrentPage with
    | Home -> 
        Html.div [
            prop.className "wrapper"
            prop.children [
                Html.form [
                    prop.onSubmit (fun _ -> dispatch (SwitchPage Page.Overview))
                    prop.className "wrapper-login"
                    prop.children [
                        Html.h1 "OGame"
                        Html.h2 "Clone with F#"
                        Html.input [
                            prop.placeholder "Username"
                            prop.required true
                            prop.valueOrDefault "sylviot"
                            //prop.onChange (fun _ -> SetUsernameInput >> dispacth)
                        ]
                        Html.button [
                            prop.className "btn btn-primary"
                            prop.type' "submit"
                            //prop.onClick (fun _ -> dispatch (SwitchPage Page.Overview))
                            prop.text "Start game"
                        ]
                    ]
                ]
            ]
        ]
    | Overview ->
        Html.div [
            Html.h1 "Overview "

            Menu(state, dispatch)

            Html.div [
                Html.div [
                ]
                Html.span "1 field"
                Html.span "[1:0:1] Location"
                Html.span "-21C to 19C - Temperature"
            ]
        ]
    | Page.Building -> 
        Html.div [
            Html.h1 "Building"

            Menu(state, dispatch)
            
            Html.div [
                Html.h3 (sprintf "Metal mine %d" state.MetalMineLevel)
                Html.button [
                    prop.text "Upgrade"
                    prop.disabled state.Building.IsSome
                    prop.onClick (fun _ -> dispatch (BuildUpgrade Builds.MetalMine))
                ]
            ]
            Html.div [
                Html.h3 (sprintf "Crystal mine %d" state.CrystalMineLevel)
                Html.button [
                    prop.text "Upgrade"
                    prop.disabled state.Building.IsSome
                    prop.onClick (fun _ -> dispatch (BuildUpgrade Builds.CrystalMine))
                ]
            ]
            Html.div [
                Html.h3 (sprintf "Deuterium mine %d" state.DeuteriumMineLevel)
                Html.button [
                    prop.text "Upgrade"
                    prop.disabled state.Building.IsSome
                    prop.onClick (fun _ -> dispatch (BuildUpgrade Builds.DeuteriumMine))
                ]
            ]
            Html.div [
                Html.h3 (sprintf "Research %d" state.ResearchLevel)
                Html.button [
                    prop.text "Upgrade"
                    prop.disabled state.Building.IsSome
                    prop.onClick (fun _ -> dispatch (BuildUpgrade Builds.Research))
                ]
            ]
            Html.div [
                Html.h3 (sprintf "Shipyard %d" state.ShipyardLevel)
                Html.button [
                    prop.text "Upgrade"
                    prop.disabled state.Building.IsSome
                    prop.onClick (fun _ -> dispatch (BuildUpgrade Builds.Shipyard))
                ]
            ]
        ]
    | Page.Research -> Html.h1 "Research"
    | Page.Shipyard -> 
        Html.div [
            Menu(state, dispatch)
            Html.h1 (sprintf "Shipyard %d" state.ShipyardLevel)
            if state.ShipyardLevel > 0 then
                Html.h2 "Listagem"
            else
                Html.h2 "N�o tem o estaleiro ainda"
        ]
    | Defense -> Html.h1 "Defense"
    | Galaxy -> Html.h1 "Galaxy"



// Link: https://fable.io/fable-graphics/samples/pixi/basic/index.html
//let animate =
//  let mutable tick = 0.
//  let mutable last = 0
//  let rec animate (dt:float) =
//    tick <- tick + 0.016

//    if (Convert.ToInt32 tick) > last then
//        last <- Convert.ToInt32 tick
//        //JS.console.log "Time"

//    window.requestAnimationFrame(FrameRequestCallback animate) |> ignore
//  animate // Return `animate` function with `tick` trapped in a closure

//// start animating
//animate 0.


Program.mkProgram init update render
|> Program.withReactSynchronous "elmish-app"
|> Program.run