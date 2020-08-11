module Client

open Elmish
open Elmish.React
open Fable.React
open Fable.React.Props
open Fetch.Types
open Thoth.Fetch
open Thoth.Json

open Shared
open Fable.Core.JS

// The model holds data that you want to keep track of while the application is running
// in this case, we are keeping track of a counter
// we mark it as optional, because initially it will not be available from the client
// the initial value will be requested from server
type Model =
    { Counter: Counter option
      GameMap: GameMap }

// The Msg type defines what events/actions can occur while the application is running
// the state of the application changes *only* in reaction to these events
type Msg =
    | SelectHex of Axial
    | Increment
    | Decrement
    | InitialCountLoaded of Counter

let initialCounter () = Fetch.fetchAs<unit, Counter> "/api/init"

// defines the initial state and initial command (= side-effect) of the application
let init (): Model * Cmd<Msg> =
    let initialModel =
        { Counter = None
          GameMap = GameMap.init }

    let loadCountCmd =
        Cmd.OfPromise.perform initialCounter () InitialCountLoaded

    initialModel, loadCountCmd

// The update function computes the next state of the application based on the current state and the incoming events/messages
// It can also run side-effects (encoded as commands) like calling the server via Http.
// these commands in turn, can dispatch messages to which the update function will react.
let update (msg: Msg) (model: Model): Model * Cmd<Msg> =
    match msg with
    | Increment ->
        { model with Counter = model.Counter |> Option.map (fun counter -> { Value = counter.Value + 1 }) }, Cmd.none
    | Decrement ->
        { model with Counter = model.Counter |> Option.map (fun counter -> { Value = counter.Value - 1 }) }, Cmd.none
    | InitialCountLoaded initialCount ->
        { model with Counter = Some initialCount }, Cmd.none
    | SelectHex axial ->
        { model with GameMap = GameMap.toggle axial model.GameMap }, Cmd.none

let gameMap map dispatch =
    let size = GameMap.tileSize
    // let h = sqrt(3.) * (float size)
    // let w = 2. * (float size)
    // let horizontalDistance = w * 0.75

    let center = Point(500., 500.)

    // let startPoint = Point (500., 500.)
    // let nextHexStartPoint = Point(startPoint.X + horizontalDistance, startPoint.Y + (h/2.))

    let axialToPoint =
        Hex.flatHexToPixel size
        >> fun point -> center + point
        >> fun point ->
            [ 0 .. 6 ]
            |> List.map (Hex.flatHexCorner point size)
            |> List.fold (fun curr (Point (x, y)) -> sprintf "%s %f,%f" curr x y) ""

    let stateToColor =
        function
        | Empty -> "#1a3643"
        | Selected -> "#f2b07b"

    let selectAxial axe _ =
        dispatch <| SelectHex axe

    svg
        [ Id "board"
          HTMLAttr.Custom("xmlns", "http://www.w3.org/2000/svg")
          HTMLAttr.Custom("version", "1.1")
          HTMLAttr.Custom("width", "100%")
          HTMLAttr.Custom("height", "100vh")
          HTMLAttr.Custom("xmlnsXlink", "http://www.w3.org/1999/xlink") ]

        (map
         |> GameMap.toList
         |> List.map (fun (axe, state) ->
             polyline [ Class "hex"
                        HTMLAttr.Custom("stroke", "#0f1417")
                        HTMLAttr.Custom("fill", (stateToColor state))
                        HTMLAttr.Custom("points", axialToPoint axe) 
                        OnClick (selectAxial axe) ] []))

let safeComponents =
    let components =
        span [] [
            a [ Href "https://github.com/SAFE-Stack/SAFE-template" ] [
                str "SAFE  "
                str Version.template
            ]
            str ", "
            a [ Href "https://saturnframework.github.io" ] [
                str "Saturn"
            ]
            str ", "
            a [ Href "http://fable.io" ] [
                str "Fable"
            ]
            str ", "
            a [ Href "https://elmish.github.io" ] [
                str "Elmish"
            ]
        ]

    span [] [
        str "Version "
        strong [] [ str Version.app ]
        str " powered by: "
        components
    ]

let show =
    function
    | { Counter = Some counter } -> string counter.Value
    | { Counter = None } -> "Loading..."

let view (model: Model) (dispatch: Msg -> unit) =
    fragment [] [
        gameMap model.GameMap dispatch
        safeComponents
    ]

#if DEBUG
open Elmish.Debug
open Elmish.HMR
#endif

Program.mkProgram init update view
#if DEBUG
|> Program.withConsoleTrace
#endif
|> Program.withReactBatched "elmish-app"
#if DEBUG
|> Program.withDebugger
#endif
|> Program.run
