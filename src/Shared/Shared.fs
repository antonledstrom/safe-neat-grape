namespace Shared

type Counter = { Value : int }

module Calculations =
    let lerp (a: double) (b: double) (t: double) = a * (1. - t) + b * t

[<Struct>]
type FractionalAxial =
    | FractionalAxial of q: double * r: double
    static member (+)(FractionalAxial(q1, r1), FractionalAxial(q2, r2)) = FractionalAxial(q1 + q2, r1 + r2)
    static member (*)(a, FractionalAxial(q, r)) = FractionalAxial(q * a, r * a)

    member this.Q =
        match this with
        | FractionalAxial (q, _) -> q

    member this.X = this.Q

    member this.R =
        match this with
        | FractionalAxial (_, r) -> r

    member this.Y = this.R

    member this.S =
        match this with
        | FractionalAxial (q, r) -> -q - r

    member this.Z = this.S

module FractionalAxial =
    let distance (a: FractionalAxial) (b: FractionalAxial) =
        (abs (a.X - b.X)
         + abs (a.Y - b.Y)
         + abs (a.Z - b.Z))
        / 2.

[<Struct>]
type Axial =
    | Axial of q: int * r: int
    static member (+)(Axial(q1, r1), Axial(q2, r2)) = Axial(q1 + q2, r1 + r2)
    static member (-)(Axial(q1, r1), Axial(q2, r2)) = Axial(q1 - q2, r1 - r2)
    static member (*)(a, Axial(q, r)) = Axial(q * a, r * a)

    member this.Q =
        match this with
        | Axial (q, _) -> q

    member this.R =
        match this with
        | Axial (_, r) -> r

    member this.S =
        match this with
        | Axial (q, r) -> -q - r

module Axial =
    let private length (hex: Axial) =
        (abs (hex.Q) + abs (hex.R) + abs (hex.S)) / 2

    let distance (hex1: Axial) (hex2: Axial) = length (hex1 - hex2)

    // int hex_distance(Hex a, Hex b) {
    //     return hex_length(hex_subtract(a, b));
    // }
    let fromFractionalAxial (f: FractionalAxial) =
        let q = int (round (f.Q))
        let r = int (round (f.R))
        let s = int (round (f.S))

        let qDiff = abs (double q - f.Q)
        let rDiff = abs (double r - f.R)
        let sDiff = abs (double s - f.S)

        if qDiff > rDiff && qDiff > sDiff then Axial(q - r - s, r)
        elif rDiff > sDiff then Axial(q, -q - s)
        else Axial(q, r)

    let toFractional (Axial (q, r)) = FractionalAxial(double q, double r)

    let axialLerp (a: Axial) (b: Axial) distance =
        FractionalAxial
            (Calculations.lerp (double a.Q) (double b.Q) distance, Calculations.lerp (double a.R) (double b.R) distance)

    let line (hex1: Axial) (hex2: Axial) =
        // let fracational1 = toFractional hex1
        // let fracational2 = toFractional hex1
        
        let n = FractionalAxial.distance (toFractional hex1) (toFractional hex2)
        let step = 1. / max n 1.
        // let hex1Nudge = FractionalAxial (hex1.Q + 1e-6, hex2.R + 1e-6)
        // let hex1Nudge = FractionalAxial(b.q + 1e-6, b.r + 1e-6)
        // FractionalHex b_nudge(b.q + 1e-6, b.r + 1e-6, b.s - 2e-6);
        let rec fold acc i : Axial list =
            let axe =
                axialLerp hex1 hex2 (double (i * step))
                |> fromFractionalAxial

            if i < n then fold (axe::acc) (i + 1.)
            else axe::acc

        fold [] 0.
        // [ 0 .. n ]
        // |> List.map (fun i -> axialLerp hex1 hex2 (double (i * step)) |> fromFractionalAxial)

//     hex_lerp(Hex a, Hex b, double t) {
//     return FractionalHex(lerp(a.q, b.q, t),
//                          lerp(a.r, b.r, t),
//                          lerp(a.s, b.s, t));
// }
    let N = Axial(0, -1)
    let S = Axial(0, 1)
    let NW = Axial(-1, 0)
    let NE = Axial(1, -1)
    let SW = Axial(-1, 1)
    let SE = Axial(1, 0)

    let W2 = NW + SW
    let E2 = NE + SE
    let center = Axial(0, 0)

    let cube (Axial (q, r)) = q, r, -q - r

[<Struct>]
type Point =
    | Point of x: float * y: float
    member this.X =
        match this with
        | Point (x, _) -> x

    member this.Y =
        match this with
        | Point (_, y) -> y

    static member (+)(Point(q1, r1), Point(q2, r2)) = Point(q1 + q2, r1 + r2)

type Border =
    | N
    | NE
    | NW

type HexState =
    | Empty
    | NotSelectable
    | Selected

type GameMap = Map<Axial, HexState>

module GameMap =
    let tileSize = 50

    let init =
        Map.empty
        |> Map.add Axial.center Selected
        |> Map.add Axial.N NotSelectable
        |> Map.add Axial.NW NotSelectable
        |> Map.add Axial.NE NotSelectable
        |> Map.add Axial.SE NotSelectable
        |> Map.add Axial.SW NotSelectable
        |> Map.add Axial.S Empty

    let toList (m: GameMap) = Map.toList m
    let select axe = Map.add axe Selected

    let toggle axe (gameMap: Map<Axial, HexState>): Map<Axial, HexState> =
        gameMap
        |> Map.find axe
        |> (function
        | NotSelectable -> NotSelectable
        | Selected -> Empty
        | Empty -> Selected)
        |> fun s -> Map.add axe s gameMap

    let private defaultState (axe: Axial) =
        Map.tryFind axe
        >> function
        | Some s -> s
        | None -> Empty

    let addTile axe (gameMap: Map<Axial, HexState>): Map<Axial, HexState> =
        [ Axial.N + axe
          Axial.NW + axe
          Axial.NE + axe
          Axial.S + axe
          Axial.SW + axe
          Axial.SE + axe ]
        |> List.fold (fun acc a -> Map.add a (defaultState a gameMap) acc) gameMap

module Hex =
    open System

    let flatHexCorner (Point (x, y)) (size: int) (cornerNumber: int) =
        let angleDeg = 60 * cornerNumber
        let angleRad = Math.PI / 180. * (float angleDeg)

        let x =
            (float x) + (float size) * cos (angleRad)

        let y =
            (float y) + (float size) * sin (angleRad)

        Point(x, y)

    let flatHexToPixel (size: int) (axe: Axial) =
        let x = (float size) * (3. / 2. * (float axe.Q))

        let y =
            (float size)
            * (sqrt (3.)
               / 2.
               * (float axe.Q)
               + sqrt (3.) * (float axe.R))

        Point(x, y)

[<Struct>]
type Cube =
    | Cube of x: int * y: int * z: int
    member this.X =
        match this with
        | Cube (x, _, _) -> x

    member this.Y =
        match this with
        | Cube (_, y, _) -> y

    member this.Z =
        match this with
        | Cube (_, _, z) -> z

module Cube =
    let toAxial (Cube (x, _, z)) = Axial(x, z)

    let fromAxial (Axial (q, r)) =
        let y = -q - r
        Cube(q, y, r)

    let distance (a: Cube) (b: Cube) =
        (abs (a.X - b.X)
         + abs (a.Y - b.Y)
         + abs (a.Z - b.Z))
        / 2

    let hexDistance a b =
        let ac = fromAxial a
        let bc = fromAxial b

        distance ac bc

    let private lerp a b t = a + (b - a) * t

    let private cubeLerp (a: Cube) (b: Cube) (t: int) =
        Cube(lerp a.X b.X t, lerp a.Y b.Y t, lerp a.Z b.Z t)

    let linedraw a b =
        let n = distance a b

        [ 0 .. n ]
        |> List.map (fun i -> cubeLerp a b (1 / n * i))

// function cube_to_axial(cube):
//     var q = cube.x
//     var r = cube.z
//     return Hex(q, r)

// function axial_to_cube(hex):
//     var x = hex.q
//     var z = hex.r
//     var y = -x-z
//     return Cube(x, y, z)

// function cube_to_oddq(cube):
//     var col = cube.x
//     var row = cube.z + (cube.x - (cube.x&1)) / 2
//     return OffsetCoord(col, row)

// function oddq_to_cube(hex):
//     var x = hex.col
//     var z = hex.row - (hex.col - (hex.col&1)) / 2
//     var y = -x-z
//     return Cube(x, y, z)
