namespace Shared.Puzzle
open Shared

type PuzzlePiece =
    | Fixed of int
    | Valid of int
    | Invalid of int
    | Empty

module PuzzlePiece =
    let toDisplayString =
        function
        | Fixed i
        | Invalid i
        | Valid i -> string i
        | Empty -> ""

    let toDebugString (Axial (x, y)) =
        toDisplayString
        >> fun s -> sprintf "%i %s %i" x s y

type Puzzle = Map<Axial, PuzzlePiece>

module Puzzle =
    let tileSize = 50

    let init =
        Map.empty
        |> Map.add Axial.center (Fixed 5)
        |> Map.add (Axial.center + Axial(0, -1)) Empty
        |> Map.add (Axial.center + Axial(0, -2)) Empty
        |> Map.add (Axial.center + Axial(0, 1)) Empty
        |> Map.add (Axial.center + Axial(0, 2)) Empty
        |> Map.add (Axial.center + Axial(1, 1)) Empty
        |> Map.add (Axial.center + Axial(1, 0)) Empty
        |> Map.add (Axial.center + Axial(1, -1)) Empty
        |> Map.add (Axial.center + Axial(1, -2)) Empty
        |> Map.add (Axial.center + Axial(2, 0)) Empty
        |> Map.add (Axial.center + Axial(2, -1)) Empty
        |> Map.add (Axial.center + Axial(2, -2)) Empty
        |> Map.add (Axial.center + Axial(-1, 0)) Empty
        |> Map.add (Axial.center + Axial(-1, -1)) Empty
        |> Map.add (Axial.center + Axial(-1, 1)) Empty
        |> Map.add (Axial.center + Axial(-1, 2)) Empty
        |> Map.add (Axial.center + Axial(-2, 0)) Empty
        |> Map.add (Axial.center + Axial(-2, 1)) Empty
        |> Map.add (Axial.center + Axial(-2, 2)) Empty

    let toList (m: Puzzle) = Map.toList m