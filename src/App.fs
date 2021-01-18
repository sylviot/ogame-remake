module App

open Elmish
open Elmish.React
open Feliz
open Feliz.Router
open Fable.Core 
// APP

[<Literal>]
let Speed = 1.5

type Page =
    | Home
    | Overview
    | Building
    | Research
    | Shipyard
    | Defense

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
    | SetUsername of string
    | SwitchPage of Page
    | BuildingUpgrade of Builds
    | BuildingUpgraded of Builds
    | ResearchUpgrade of Tecnologies
    | ResearchUpgraded of Tecnologies
    | ShipyardBuild of Ships
    | DefenseBuild of Defenses

type MaterialCost = {Metal: int; Crystal: int; Deuterium: int;}

type State = { CurrentPage: Page; 
    Username: string;
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
    Username = "sylviot";
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

// <GLOBAL HELPERS
let delayed (command) (time: int) =
    let fn (dispatch: Command -> unit): unit = 
        let tick = async {
            do! Async.Sleep time
            dispatch command
        }
        Async.StartImmediate tick
    fn

let buildingMaterialCost (metal: int) (crystal: int) (deuterium: int) (level: int) =
    let costToBuild = pown Speed level |> int
    {Metal = (*) metal costToBuild; Crystal = (*) crystal costToBuild; Deuterium = (*) deuterium costToBuild;}

let timeToBuild (level: int) =
    level * 1000
// GLOBAL HELPERS />

let update (command: Command) (state: State) = 
    match command with
    // ToDo -> verificar uma forma de with multiplos atributos de um record
    | Tick -> {state with 
        Metal = state.Metal + (30 *state.MetalMineLevel * int (pown 1.1 state.MetalMineLevel));
        Crystal = state.Crystal + 100 + state.CrystalMineLevel * 10;
        Deuterium = state.Deuterium + 100 + state.DeuteriumMineLevel * 10;}, Cmd.ofSub (delayed Tick 1000)
    | SetUsername value -> { state with Username = value }, Cmd.none
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
        | None -> 
            match build with
            | Builds.MetalMine -> { state with Building = Some build }, Cmd.ofSub (delayed (BuildingUpgraded build) (timeToBuild state.MetalMineLevel))
            | Builds.CrystalMine -> { state with Building = Some build }, Cmd.ofSub (delayed (BuildingUpgraded build) (timeToBuild state.CrystalMineLevel))
            | Builds.DeuteriumMine -> { state with Building = Some build }, Cmd.ofSub (delayed (BuildingUpgraded build) (timeToBuild state.DeuteriumMineLevel))
            | Builds.Shipyard -> { state with Building = Some build }, Cmd.ofSub (delayed (BuildingUpgraded build) (timeToBuild state.ShipyardLevel))
            | Builds.Research -> { state with Building = Some build }, Cmd.ofSub (delayed (BuildingUpgraded build) (timeToBuild state.ResearchLevel))
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
let MenuItem (text: string, page: Page, (state: State), dispatch) = Html.div [
    prop.className "menu-item"
    prop.text text
    prop.onClick (fun _ -> dispatch (SwitchPage page))
]
let Menu (state: State, dispatch) = Html.div [
    prop.className "menu"
    prop.children [
        MenuItem ("Overview", Page.Overview, state, dispatch)
        MenuItem ("Building", Page.Building, state, dispatch)
        MenuItem ("Research", Page.Research, state, dispatch)
        MenuItem ("Shipyard", Page.Shipyard, state, dispatch)
        MenuItem ("Defense", Page.Defense, state, dispatch)
    ]
]

let Info (state: State, dispatch) = Html.div [
    prop.className "d-flex"
    prop.children [
        Html.div [ 
            prop.className "d-flex-item"
            prop.children [
                Html.span [
                    prop.className "d-block"
                    prop.text state.Metal
                ]
                Html.span [
                    prop.className "d-block"
                    prop.text "Metal"
                ]
            ]
        ]
        Html.div [ 
            prop.className "d-flex-item"
            prop.children [
                Html.span [
                    prop.className "d-block"
                    prop.text state.Crystal
                ]
                Html.span [
                    prop.className "d-block"
                    prop.text "Crystal"
                ]
            ]
        ]
        Html.div [ 
            prop.className "d-flex-item"
            prop.children [
                Html.span [
                    prop.className "d-block"
                    prop.text state.Deuterium
                ]
                Html.span [
                    prop.className "d-block"
                    prop.text "Deuterium"
                ]
            ]
        ]
    ]
]


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

let Item (image: string) (name: string) (description: string) (metal: int) (crystal: int) (deuterium: int) actionButton = Html.div [
    prop.className "option-item"
    prop.children [
        Html.img [ prop.src image ]
        Html.div [
            prop.className "option-item-info"
            prop.children [
                Html.span [
                    prop.className "option-item-title"
                    prop.text name
                ]
                Html.span [
                    prop.className "option-item-description"
                    prop.text description
                ]
                Html.div [
                    prop.className "option-item-material"
                    prop.children [
                        Html.div [
                            Html.span metal
                            Html.span "Metal"
                        ]
                        Html.div [
                            Html.span crystal
                            Html.span "Crystal"
                        ]
                        Html.div [
                            Html.span deuterium
                            Html.span "Deuterium"
                        ]
                        actionButton
                    ]
                ]
            ]
        ]
    ]
]

let BuildingItem (state: State) (dispatch: Command -> unit) (image: string) (name: string) (description: string) (building: Builds) (material: MaterialCost) = 
    let button = Html.div [
        Html.button [
            prop.text "Evolve"
            prop.disabled state.Building.IsSome
            prop.onClick (fun _ -> dispatch (Command.BuildingUpgrade building))
        ]
    ]
    Item image name description material.Metal material.Crystal material.Deuterium button


let ResearchItem (state: State) (dispatch: Command -> unit) (image: string) (name: string) (description: string) (tech: Tecnologies) =
    let button = Html.div [
        Html.button [
            prop.text "Evolve"
            prop.disabled state.Researching.IsSome
            prop.onClick (fun _ -> dispatch (ResearchUpgrade tech))
        ]
    ]
    Item image name description 0 0 0 button

let getTime (value: float) : string =
    let d = System.TimeSpan.FromMilliseconds value
    (int d.TotalMinutes).ToString("00") + ":" + (d.TotalSeconds % 60.0).ToString("00")
// COMPONENTS />

let render (state: State) (dispatch: Command -> unit) =
    match state.CurrentPage with
    | Page.Home -> 
        Html.div [
            prop.className "login-wrapper"
            prop.children [
                Html.form [
                    prop.onSubmit (fun _ -> dispatch (SwitchPage Page.Overview))
                    prop.className "login"
                    prop.children [
                        Html.h1 "OGame"
                        Html.h2 "Remake with F#"
                        Html.input [
                            prop.autoFocus true
                            prop.placeholder "Username"
                            prop.required true
                            prop.onChange (fun value -> dispatch (SetUsername value))
                        ]
                        Html.button [
                            prop.className "btn btn-primary"
                            prop.type' "submit"
                            prop.disabled (System.String.IsNullOrEmpty state.Username)
                            prop.text "Start game"
                        ]
                    ]
                ]
            ]
        ]
    | Page.Overview -> Template(state, dispatch, Html.div [
        prop.className "d-flex d-flex-column"
        prop.children [
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
                ]
            ]
            Html.span [ 
                prop.className "text-center"
                prop.text (sprintf "Planet with %d fields" (state.MetalMineLevel + state.CrystalMineLevel + state.DeuteriumMineLevel)) 
            ]
            Html.span [
                prop.className "text-center"
                prop.text "Location on [1:0:1]"
            ]
            Html.span [
                prop.className "text-center"
                prop.text "Temperature -21C to 19C"
            ]
        ]
    ])
    | Page.Building -> Template(state, dispatch, Html.div [
        Html.h1 "Building"
        Html.div [
            prop.className "options"
            prop.children [
                BuildingItem state dispatch "images/metal-mine.gif" "Metal mine" (sprintf "%s to level %d"  (getTime <| float (timeToBuild state.MetalMineLevel)) ((+)state.MetalMineLevel 1)) Builds.MetalMine (buildingMaterialCost 60 48 0 state.MetalMineLevel)
                BuildingItem state dispatch "images/crystal-mine.gif" "Crystal mine" (sprintf "%s to level %d" (getTime <| float (timeToBuild state.CrystalMineLevel)) ((+)state.CrystalMineLevel 1)) Builds.CrystalMine (buildingMaterialCost 48 24 0 state.CrystalMineLevel)
                BuildingItem state dispatch "images/deuterium-refinery.gif" "Deuterium mine" (sprintf "%s to level %d" (getTime <| float (timeToBuild state.DeuteriumMineLevel)) ((+)state.DeuteriumMineLevel 1)) Builds.DeuteriumMine (buildingMaterialCost 225 75 0 state.DeuteriumMineLevel)
                BuildingItem state dispatch "images/shipyard.gif" "Shipyard lab" (sprintf "%s to level %d" (getTime <| float (timeToBuild state.ShipyardLevel)) ((+)state.ShipyardLevel 1)) Builds.Shipyard (buildingMaterialCost 200 400 200 state.ShipyardLevel)
                BuildingItem state dispatch "images/researchlab.gif" "Research lab" (sprintf "%s to level %d" (getTime <| float (timeToBuild state.ResearchLevel)) ((+)state.ResearchLevel 1)) Builds.Research (buildingMaterialCost 200 400 200 state.ResearchLevel)
            ]
        ]
    ])
    | Page.Research -> Template(state, dispatch, Html.div [
        Html.h1 "Research"
        if state.ResearchLevel > 0 then
            Html.div [
                prop.className "options"
                prop.children [
                    ResearchItem state dispatch "images/armour-tech.gif" "Armour tecnology" (sprintf "00:01 to level %d" ((+)state.ArmourTecnologyLevel 1)) Tecnologies.Armour
                    ResearchItem state dispatch "images/weapon-tech.gif" "Weapon tecnology" (sprintf "00:01 to level %d" ((+)state.WeaponTecnologyLevel 1)) Tecnologies.Weapon
                    ResearchItem state dispatch "images/shield-tech.gif" "Shield tecnology" (sprintf "00:01 to level %d" ((+)state.DefenseTecnologyLevel 1)) Tecnologies.Defense
                    ResearchItem state dispatch "images/laser-tech.gif" "Laser tecnology" (sprintf "00:01 to level %d" ((+)state.LaserTecnologyLevel 1)) Tecnologies.Laser
                ]
            ]
        else
            Html.h2 "The reaserach lab not built yet!"
    ])
    | Page.Shipyard -> Template(state, dispatch, Html.div [
        Html.h1 "Shipyard"
        if state.ShipyardLevel > 0 then
            Html.h2 "This is an example only! :)"
        else
            Html.h2 "The shipyard is not built yet!"
    ])
    | Page.Defense -> Template(state, dispatch, Html.div [
        Html.h1 "Defense"
        if state.ShipyardLevel > 0 && state.ResearchLevel > 0 then
            Html.h2 "This is an example only! :)"
        else
            Html.h2 "Build shipyard and research new technologies!"
    ])

Program.mkProgram init update render
|> Program.withReactSynchronous "elmish-app"
|> Program.run



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