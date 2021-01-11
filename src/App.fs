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

type Builds =
    | MetalMine
    | CrystalMine
    | DeuteriumMine
    | Research
    | Shipyard

type Tecnologies =
    | Spy
    | Armour
    | Weapon
    | Defense
    | Laser
    | Ion
    | Plasma
    | Combustion
    | Impulsion

type Ships =
    | LightFighter

type Defenses =
    | LightLaserTurret

type Command =
    | Tick
    | SwitchPage of Page
    | BuildingUpgrade of Builds
    | BuildingUpgraded of Builds
    | ResearchUpgrade of Tecnologies
    | ResearchUpgraded of Tecnologies
    | ShipyardBuild of Ships
    | DefenseBuild of Defenses


type State = { CurrentPage: Page; 
    Metal: int; Crystal: int; Deuterium: int;
    MetalMineLevel: int; CrystalMineLevel: int; DeuteriumMineLevel: int;
    ResearchLevel: int; 
    SpyTecnologyLevel: int; 
    ArmourTecnologyLevel: int; WeaponTecnologyLevel: int; DefenseTecnologyLevel: int; 
    LaserTecnologyLevel: int; IonTecnologyLevel: int; PlasmaTecnologyLevel: int; 
    CombustionTecnologyLevel: int; ImpulsionTecnologyLevel: int;
    ShipyardLevel: int;
    LightFighterQuantity: int;
    LightLaserTurretQuantity: int;
    Building: Builds option; Researching: Tecnologies option; }

let init() = { 
    CurrentPage = Home;
    Metal = 0; Crystal = 0; Deuterium = 0;
    MetalMineLevel =  1; CrystalMineLevel = 1; DeuteriumMineLevel = 1;
    ResearchLevel = 0;
    SpyTecnologyLevel = 0;
    ArmourTecnologyLevel = 0; WeaponTecnologyLevel = 0; DefenseTecnologyLevel = 0;
    LaserTecnologyLevel = 0; IonTecnologyLevel = 0; PlasmaTecnologyLevel = 0;
    CombustionTecnologyLevel = 0; ImpulsionTecnologyLevel = 0;
    ShipyardLevel = 0;
    LightFighterQuantity = 0; LightLaserTurretQuantity = 0;
    Building = None; Researching = None; }, Cmd.none

let update (command: Command) (state: State) = 
// ToDo - delayed pode ser alterado para delayedDispatchCommand -> para receber um Command para dispatch com delay também passado por parametro
    let delayed (command) (time: int) =
        let fn (dispatch: Command -> unit): unit = 
            let tick = async {
                do! Async.Sleep time
                dispatch command
            }
            Async.StartImmediate tick
        fn

    match command with
    // ToDo -> verificar uma forma de with multiplos atributos de um record
    | Tick -> {state with 
        Metal = state.Metal + 100 + state.MetalMineLevel * 10;
        Crystal = state.Crystal + 100 + state.CrystalMineLevel * 10;
        Deuterium = state.Deuterium + 100 + state.DeuteriumMineLevel * 10;}, Cmd.ofSub (delayed Tick 1000)
    | SwitchPage nextPage ->
        match nextPage with
        | Overview -> { state with CurrentPage = nextPage }, Cmd.ofSub (delayed Tick 1000)
        | _ -> { state with CurrentPage = nextPage }, Cmd.none
    | BuildingUpgraded build ->
        match build with
        | MetalMine -> { state with MetalMineLevel = state.MetalMineLevel + 1; Building = None }, Cmd.none
        | CrystalMine -> { state with CrystalMineLevel = state.CrystalMineLevel + 1; Building = None }, Cmd.none
        | DeuteriumMine -> { state with DeuteriumMineLevel = state.DeuteriumMineLevel + 1; Building = None }, Cmd.none
        | Research -> { state with ResearchLevel = state.ResearchLevel + 1; Building = None }, Cmd.none
        | Shipyard -> { state with ShipyardLevel = state.ShipyardLevel + 1; Building = None }, Cmd.none
    | BuildingUpgrade build -> 
        match state.Building with
        | None -> { state with Building = Some build }, Cmd.ofSub (delayed (BuildingUpgraded build) 1000)
        | Some -> state, Cmd.none
    | ResearchUpgraded tech -> 
        match tech with
        | Spy -> {state with SpyTecnologyLevel = state.SpyTecnologyLevel + 1; Researching = None; }, Cmd.none
        | Armour -> {state with ArmourTecnologyLevel = state.ArmourTecnologyLevel + 1; Researching = None; }, Cmd.none
        | Weapon -> {state with WeaponTecnologyLevel = state.WeaponTecnologyLevel + 1; Researching = None; }, Cmd.none
        | Defense -> {state with DefenseTecnologyLevel = state.DefenseTecnologyLevel + 1; Researching = None; }, Cmd.none
        | Laser -> {state with LaserTecnologyLevel = state.LaserTecnologyLevel + 1; Researching = None; }, Cmd.none
        | Ion -> {state with IonTecnologyLevel = state.IonTecnologyLevel + 1; Researching = None; }, Cmd.none
        | Plasma -> {state with PlasmaTecnologyLevel = state.PlasmaTecnologyLevel + 1; Researching = None; }, Cmd.none
        | Combustion -> {state with CombustionTecnologyLevel = state.CombustionTecnologyLevel + 1; Researching = None; }, Cmd.none
        | Impulsion -> {state with ImpulsionTecnologyLevel = state.ImpulsionTecnologyLevel + 1; Researching = None; }, Cmd.none
    | ResearchUpgrade tech ->
        match state.Researching with
        | None -> { state with Researching = Some tech }, Cmd.ofSub (delayed (ResearchUpgraded tech) 1000)
        | Some -> state, Cmd.none
    | ShipyardBuild -> { state with LightFighterQuantity = state.LightFighterQuantity + 1}, Cmd.none
    | DefenseBuild -> { state with LightLaserTurretQuantity = state.LightLaserTurretQuantity + 1}, Cmd.none

// <COMPONENTS
let MenuItem (text: string, page: Page,  dispatch) = Html.div [
    prop.className "menu-item"
    prop.text text
    prop.onClick (fun _ -> dispatch (SwitchPage page))
]
let Menu (state: State, dispatch) = Html.div [
    prop.className "menu"
    prop.children [
        MenuItem ("Overview", Page.Overview, dispatch)
        MenuItem ("Building", Page.Building, dispatch)
        MenuItem ("Research", Page.Research, dispatch)
        MenuItem ("Galaxy", Page.Galaxy, dispatch)
        MenuItem ("Shipyard", Page.Shipyard, dispatch)
        MenuItem ("Defense", Page.Defense, dispatch)
    ]
]

let Info (state: State, dispatch) = Html.div [
    prop.className "d-flex"
    prop.children [
        Html.span [ 
            prop.className "d-flex-item"
            prop.text (sprintf "Metal %d" state.Metal)
        ]
        Html.span [
            prop.className "d-flex-item"
            prop.text (sprintf "Crystal %d" state.Crystal)
        ]
        Html.span [
            prop.className "d-flex-item"
            prop.text (sprintf "Deuterium %d" state.Deuterium)
        ]
    ]
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

let Template (state: State, dispatch: Command -> unit, content) = Html.div [
    prop.className "d-flex app"
    prop.children [
        Html.div [
            prop.className "d-flex-item sidebar-menu"
            prop.children [ Menu(state, dispatch) ]
        ]
        Html.div [
            prop.className "d-flex-item content"
            prop.children [ content ]
        ]
        Html.div [
            prop.className "d-flex-item sidebar-info"
            prop.children [ Info(state, dispatch) ]
        ]
    ]
]

let ResearchItem (researching: bool) (dispatch) (name:string) (quantity: int) (tech: Tecnologies) = Html.div [
    Html.h3 (sprintf "%s %d" name quantity)
    Html.button [
        prop.text "Research"
        prop.disabled researching
        prop.onClick (fun _ -> dispatch (ResearchUpgrade tech))
    ]
]
// COMPONENTS />

let render (state: State) (dispatch: Command -> unit) =
    match state.CurrentPage with
    | Page.Home -> 
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
    | Page.Overview -> Template(state, dispatch, Html.div [
        Html.h1 "Overview "
        Html.div [
            prop.className "planet-wrapper"
            prop.children [
                Html.div [
                    prop.className "planet"
                    prop.children [
                        Html.div [
                            prop.className "wrap"
                            prop.children [
                                Html.div [ prop.className "background" ]
                                Html.div [ prop.className "clouds" ]
                            ]
                        ]
                    ]
                ]
                Html.span (sprintf "%d field" (state.MetalMineLevel + state.CrystalMineLevel + state.DeuteriumMineLevel))
                Html.span "[1:0:1] Location"
                Html.span "-21C to 19C - Temperature"
            ]
        ]
    ])
    | Page.Building -> Template(state, dispatch, Html.div [
        Html.h1 "Building"
        Html.div [
            Html.h3 (sprintf "Metal mine %d" state.MetalMineLevel)
            Html.button [
                prop.text "Upgrade"
                prop.disabled state.Building.IsSome
                prop.onClick (fun _ -> dispatch (BuildingUpgrade Builds.MetalMine))
            ]
        ]
        Html.div [
            Html.h3 (sprintf "Crystal mine %d" state.CrystalMineLevel)
            Html.button [
                prop.text "Upgrade"
                prop.disabled state.Building.IsSome
                prop.onClick (fun _ -> dispatch (BuildingUpgrade Builds.CrystalMine))
            ]
        ]
        Html.div [
            Html.h3 (sprintf "Deuterium mine %d" state.DeuteriumMineLevel)
            Html.button [
                prop.text "Upgrade"
                prop.disabled state.Building.IsSome
                prop.onClick (fun _ -> dispatch (BuildingUpgrade Builds.DeuteriumMine))
            ]
        ]
        Html.div [
            Html.h3 (sprintf "Research %d" state.ResearchLevel)
            Html.button [
                prop.text "Upgrade"
                prop.disabled state.Building.IsSome
                prop.onClick (fun _ -> dispatch (BuildingUpgrade Builds.Research))
            ]
        ]
        Html.div [
            Html.h3 (sprintf "Shipyard %d" state.ShipyardLevel)
            Html.button [
                prop.text "Upgrade"
                prop.disabled state.Building.IsSome
                prop.onClick (fun _ -> dispatch (BuildingUpgrade Builds.Shipyard))
            ]
        ]
    ])
    | Page.Research -> Template(state, dispatch, Html.div [
        Html.h1 "Research"
        if state.ResearchLevel > 0 then
            Html.div [
                ResearchItem state.Researching.IsSome dispatch "Spy tecnology" state.SpyTecnologyLevel Tecnologies.Spy
                ResearchItem state.Researching.IsSome dispatch "Armour tecnology" state.ArmourTecnologyLevel Tecnologies.Armour
                ResearchItem state.Researching.IsSome dispatch "Weapon tecnology" state.WeaponTecnologyLevel Tecnologies.Weapon
                ResearchItem state.Researching.IsSome dispatch "Defense tecnology" state.DefenseTecnologyLevel Tecnologies.Defense
                ResearchItem state.Researching.IsSome dispatch "Laser tecnology" state.LaserTecnologyLevel Tecnologies.Laser
                ResearchItem state.Researching.IsSome dispatch "Ion tecnology" state.IonTecnologyLevel Tecnologies.Ion
                ResearchItem state.Researching.IsSome dispatch "Plasma tecnology" state.PlasmaTecnologyLevel Tecnologies.Plasma
                ResearchItem state.Researching.IsSome dispatch "Combustion tecnology" state.CombustionTecnologyLevel Tecnologies.Combustion
                ResearchItem state.Researching.IsSome dispatch "Impulsion tecnology" state.ImpulsionTecnologyLevel Tecnologies.Impulsion
            ]
        else
            Html.h2 "O laboratório não foi construído!"
    ])
    | Page.Shipyard -> Template(state, dispatch, Html.div [
        Html.h1 (sprintf "Shipyard %d" state.ShipyardLevel)
        if state.ShipyardLevel > 0 then
            Html.div [
                Html.h3 (sprintf "Light Fighter %d" state.LightFighterQuantity)
                Html.button [
                    prop.text "Build"
                    //prop.disabled state.Researching.IsSome // ToDo - adicionar o delayed para que funcione
                    prop.onClick (fun _ -> dispatch (ShipyardBuild Ships.LightFighter))
                ]
            ]
        else
            Html.h2 "O estaleiro ainda não foi construído!"
    ])
    | Page.Defense -> Template(state, dispatch, Html.div [
        Html.h1 "Defense"
        if state.ShipyardLevel > 0 && state.ResearchLevel > 0 then
            Html.div [
                Html.h3 (sprintf "Light Laser Turret %d" state.LightLaserTurretQuantity)
                Html.button [
                    prop.text "Build"
                    //prop.disabled state.Researching.IsSome // ToDo - adicionar o delayed para que funcione
                    prop.onClick (fun _ -> dispatch (DefenseBuild Defenses.LightLaserTurret))
                ]
            ]
        else
            Html.h2 "Construa um estaleiro e desenvolva tecnologia!"
    ])
    | Page.Galaxy -> Template(state, dispatch, Html.div [
        Html.h1 "Galaxy"
    ])



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