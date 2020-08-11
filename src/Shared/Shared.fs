namespace Shared

type Counter = { Value : int }

(*
     __
    /  \____
    \__/q  r\
       \____/
                      *)

[<Struct>]
type Axial = Axial of q:int * r:int
    with
    static member(+) (Axial(q1, r1), Axial(q2, r2)) =
        Axial (q1 + q2, r1 + r2)
    static member( * ) (a, Axial(q, r)) =
        Axial(q*a, r*a)
    member this.Q = match this with Axial(q,_) -> q
    member this.R = match this with Axial(_,r) -> r
    member this.Qf = match this with Axial(q,_) -> float q
    member this.Rf = match this with Axial(_,r) -> float r

module Axial =
    let N = Axial(0, -1)
    let S = Axial(0, +1)
    let NW = Axial(-1, 0)
    let NE = Axial(+1, -1)
    let SW = Axial(-1, +1)
    let SE  = Axial (+1, 0)

    let W2 = NW + SW
    let E2 = NE + SE
    let center = Axial(0,0)

    let cube (Axial(q,r)) =
        q,r,-q-r

[<Struct>]
type Point = Point of x:float * y:float
    with
    member this.X = match this with Point(x,_) -> x
    member this.Y = match this with Point(_,y) -> y

    static member(+) (Point(q1, r1), Point(q2, r2)) =
        Point (q1 + q2, r1 + r2)

type HexState =
    | Empty
    | Selected

type GameMap = Map<Axial, HexState>

module GameMap =
    let tileSize = 50
    let init =
        Map.empty
        |> Map.add Axial.center Empty
        |> Map.add Axial.N Empty
        |> Map.add Axial.NW Empty
        |> Map.add Axial.NE Empty
        |> Map.add Axial.SE Empty
        |> Map.add Axial.SW Empty
        |> Map.add Axial.S Empty

    let toList (m: GameMap) =
        Map.toList m

    let select axe =
        Map.add axe Selected

    let toggle axe (gameMap: Map<Axial, HexState>) : Map<Axial, HexState> =
        gameMap
        |> Map.find axe
        |> (function
            | Selected -> Empty
            | Empty -> Selected)
        |> fun s -> Map.add axe s gameMap

module Hex =
    open System
    let flatHexCorner (Point(x, y)) (size: int) (cornerNumber: int) =
        let angleDeg = 60 * cornerNumber
        let angleRad = Math.PI / 180. * (float angleDeg)

        let x = (float x) + (float size) * cos(angleRad)
        let y = (float y) + (float size) * sin(angleRad)

        Point(x, y)

    let flatHexToPixel (size: int) (axe: Axial) =
        let x = (float size) * (3./2. * axe.Qf)
        let y = (float size) * (sqrt(3.)/2. * axe.Qf + sqrt(3.) * axe.Rf)

        Point(x, y)