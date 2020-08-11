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
    
// type CrossroadSide = CLeft | CRight

// [<Struct>]
// type Crossroad = Crossroad of tile:Axial * side:CrossroadSide

// type BorderSide = BNW | BN | BNE // Remove b, 

// (*
     
//         ____
//  BNW   /q  r\
//       /      \
//       \      /
//        \____/
//                       *)

// [<Struct>]
// type Path = Path of tile:Axe * border:BorderSide

// type Direction = Up | Down | Horizontal

// type HexPoint = HexPoint of x:int * y:int
// type Hex =
//     { TopLeft: HexPoint
//       TopRigh: HexPoint
//       MidLeft: HexPoint
//       MidRight: HexPoint
//       BottomLeft: HexPoint
//       BottomRight: HexPoint }

[<Struct>]
type Point = Point of x:float * y:float
    with
    member this.X = match this with Point(x,_) -> x
    member this.Y = match this with Point(_,y) -> y

type GameMap = Axial Set

module GameMap =
    let titleSize = 100
    let init =
        Set.empty
        |> Set.add Axial.center
        |> Set.add Axial.N
        |> Set.add Axial.NW
        |> Set.add Axial.NE
        |> Set.add Axial.SE
        |> Set.add Axial.SW
        |> Set.add Axial.S

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