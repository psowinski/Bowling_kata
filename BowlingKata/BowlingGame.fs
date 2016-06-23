namespace BowlingKata
open System

module BowlingGame =

    type Rolls =
    | Roll
    | Strike (*X*)
    | Spare (*/*)

    type Pins =
    | Pins of int

    type Game = 
    | Game of (Rolls * Pins) list

    let parseGame result =
        let rec calcSpares rolls = 
            match rolls with
            | [] -> []
            | (Roll, Pins n)::(Spare,  _)::xs -> (Roll, Pins n)::(Spare, Pins (10 - n))::(calcSpares xs)
            | x::xs -> x::(calcSpares xs)

        result |> Seq.map (fun x -> match x with
                                    | 'X' -> Strike, Pins 10
                                    | '/' -> Spare, Pins -1
                                    | '-' -> Roll, Pins 0
                                    | v -> Roll, Pins (Int32.Parse(v.ToString())))
        |> Seq.toList
        |> calcSpares
        |> Game

    let scoreGame g =
        let concatIfNotEmpty x xs = 
            if List.isEmpty xs then xs else x @ xs
            
        let rec scoreRolls sum rolls =
            match rolls with
            | [] -> sum
            | (Strike, Pins p1)::(r2, Pins p2)::(r3, Pins p3)::xs -> 
                scoreRolls (sum + p1 + p2 + p3) (concatIfNotEmpty [r2, Pins p2; r3, Pins p3] xs)
            | (Spare, Pins p1)::(r2, Pins p2)::xs -> 
                scoreRolls (sum + p1 + p2) (concatIfNotEmpty [r2, Pins p2] xs)
            | (_, Pins p1)::xs -> 
                scoreRolls (sum + p1) xs

        g |> fun (Game rolls) -> scoreRolls 0 rolls
