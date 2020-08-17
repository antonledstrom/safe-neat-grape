module Client

open Elmish
open Elmish.React
open Fable.React
open Fable.React.Props
open Fetch.Types
open Thoth.Fetch
open Thoth.Json

open Shared
open Shared.Puzzle
open Fable.Core.JS

type Model =
    { Puzzle: Puzzle
      Selected: Axial
      Hover: Axial option }

type Msg =
    | SelectHex of Axial
    | HoverHex of Axial

let init (): Model * Cmd<Msg> =
    { Puzzle = Puzzle.init
      Selected = Axial.center
      Hover = None }, Cmd.none

let update (msg: Msg) (model: Model): Model * Cmd<Msg> =
    match msg with
    | SelectHex axial ->
        { model with Selected = axial }, Cmd.none
    | HoverHex axial ->
        { model with Hover = Some axial }, Cmd.none
// let gameMap map dispatch =
//     let size = GameMap.tileSize
//     // let h = sqrt(3.) * (float size)
//     // let w = 2. * (float size)
//     // let horizontalDistance = w * 0.75

//     let center = Point(500., 500.)

//     // let startPoint = Point (500., 500.)
//     // let nextHexStartPoint = Point(startPoint.X + horizontalDistance, startPoint.Y + (h/2.))

//     let axialToPoint =
//         Hex.flatHexToPixel size
//         >> fun point -> center + point
//         >> fun point ->
//             [ 0 .. 6 ]
//             |> List.map (Hex.flatHexCorner point size)
//             |> List.fold (fun curr (Point (x, y)) -> sprintf "%s %f,%f" curr x y) ""

//     let stateToColor =
//         function
//         | Empty -> "#1a3643"
//         | Selected -> "#f2b07b"
//         | NotSelectable -> "#f0f0e8"

//     let stateToBorderColor =
//         function
//         | Empty -> "#0f1417"
//         | Selected -> "#f2b07b"
//         | NotSelectable -> "#f0f0e8"

//     let selectAxial axe hexState _ =
//         match hexState with
//         | Empty
//         | Selected ->
//             SelectHex axe |> dispatch
//         | NotSelectable -> ()

//     svg
//         [ Id "board"
//           HTMLAttr.Custom("xmlns", "http://www.w3.org/2000/svg")
//           HTMLAttr.Custom("version", "1.1")
//           HTMLAttr.Custom("width", "100%")
//           HTMLAttr.Custom("height", "100vh")
//           HTMLAttr.Custom("xmlnsXlink", "http://www.w3.org/1999/xlink") ]

//         (map
//          |> GameMap.toList
//          |> List.map (fun (axe, state) ->
//              polyline [ Class "hex"
//                         HTMLAttr.Custom("stroke", (stateToBorderColor state))
//                         HTMLAttr.Custom("fill", (stateToColor state))
//                         HTMLAttr.Custom("points", axialToPoint axe)
//                         OnClick (selectAxial axe state) ] []))

let drawPuzzle selected hovered puzzleMap dispatch =
    let size = Puzzle.tileSize
    let center = Point(500., 500.)

    let axialToHexPixelPoints =
        Hex.flatHexToPixel size
        >> fun point -> center + point
        >> fun point ->
            [ 0 .. 6 ]
            |> List.map (Hex.flatHexCorner point size)
            |> List.fold (fun curr (Point (x, y)) -> sprintf "%s %f,%f" curr x y) ""

    let axialToPixelPoint =
        let w = (float size) / 2.

        Hex.flatHexToPixel size
        >> fun point -> center + point
        >> fun (Point(x, y)) -> Point (x - w, y)
    // <text font-size="{{ fontsize }}"
    //           fill="{{ color }}"
    //           font-family="Verdana"
    //           x="{{ xpos }}"
    //           y="{{ ypos }}">{{ num }}</text>
    // let stateToColor =
    //     function
    //     | Empty -> "#1a3643"
    //     | Selected -> "#f2b07b"
    //     | NotSelectable -> "#f0f0e8"

    // let stateToBorderColor =
    //     function
    //     | Empty -> "#0f1417"
    //     | Selected -> "#f2b07b"
    //     | NotSelectable -> "#f0f0e8"

    // let selectAxial axe hexState _ =
    //     match hexState with
    //     | Empty
    //     | Selected ->
    //         SelectHex axe |> dispatch
    //     | NotSelectable -> ()

    let line =
        match hovered with
        | Some axe -> Axial.line selected axe
        | _ -> []

    let selectAxial axe _ =
        dispatch <| SelectHex axe

    let hoverAxial axe _ =
        dispatch <| HoverHex axe

    let fillColor axe =
        if List.contains axe line then
            "#6bc4d2"
        else "#f0f0e8"

    fragment [] [

        div [] (line |> List.mapi (fun i a -> str <| sprintf "%i r: %i q: %i |" i a.R a.Q))
        svg
            [ Id "board"
              HTMLAttr.Custom("xmlns", "http://www.w3.org/2000/svg")
              HTMLAttr.Custom("version", "1.1")
              HTMLAttr.Custom("width", "100%")
              HTMLAttr.Custom("height", "100vh")
              HTMLAttr.Custom("xmlnsXlink", "http://www.w3.org/1999/xlink") ]

            (puzzleMap
             |> Puzzle.toList
             |> List.map (fun (axe, state) ->
                fragment [] [
                    polyline [ Class "hex"
                               HTMLAttr.Custom("stroke", "#0f1417")
                               HTMLAttr.Custom("fill", fillColor axe)
                               HTMLAttr.Custom("points", axialToHexPixelPoints axe)
                               OnClick (selectAxial axe)
                               OnMouseOver (hoverAxial axe) ] [ ]
                    text [ HTMLAttr.Custom("fill", "#0f1417")
                           HTMLAttr.Custom("fontFamily", "Verdana")
                           HTMLAttr.Custom("x", (axialToPixelPoint axe).X)
                           HTMLAttr.Custom("y", (axialToPixelPoint axe).Y) ] [ str <| PuzzlePiece.toDebugString axe state ] ])) ]

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

let drawPuzzlePiece (i: int) =
    let size = Puzzle.tileSize

    let h = sqrt(3.) * (float size)
    let w = 2. * (float size)

    let center = Point(w/2. + 1.5, h/2. + 1.)
    
    let axialToHexPixelPoints =
        Hex.flatHexToPixel size (Axial(0, 0))
        |> fun point -> center + point
        |> fun point ->
            [ 0 .. 6 ]
            |> List.map (Hex.flatHexCorner point size)
            |> List.fold (fun curr (Point (x, y)) -> sprintf "%s %f,%f" curr x y) ""

    svg
        [ HTMLAttr.Custom("xmlns", "http://www.w3.org/2000/svg")
          HTMLAttr.Custom("version", "1.1")
          HTMLAttr.Custom("width", "150px")
          HTMLAttr.Custom("height", "100px")
          HTMLAttr.Custom("xmlnsXlink", "http://www.w3.org/1999/xlink") ]
        [
            polyline [ HTMLAttr.Custom("stroke", "#0f1417")
                       HTMLAttr.Custom("fill", "#f0f0e8")
                       HTMLAttr.Custom("points", axialToHexPixelPoints ) ] [ ] ]
let menu =
    ul [ Class "puzzle-menu" ] [
        yield! [ 1..4 ] |> List.map (fun i -> li [] [ drawPuzzlePiece i ])
        yield! [ 6..18 ] |> List.map (fun i -> li [] [ drawPuzzlePiece i ]) ]

//  <div class="parent">
//     <div class="section yellow" contenteditable>
//     Min: 150px / Max: 25%
//     </div>
//     <div class="section purple" contenteditable>
//       This element takes the second grid position (1fr), meaning
//       it takes up the rest of the remaining space.
//     </div>
//   </div>
let view (model: Model) (dispatch: Msg -> unit) =
    fragment [] [
        main [] [
            div [ Class "sidebar" ] [ menu ]
            div [ Class "conent" ] [ drawPuzzle model.Selected model.Hover model.Puzzle dispatch ]
        ]
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
