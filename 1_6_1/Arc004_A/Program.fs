type Scanner() = class
    let mutable hold = [||]
    let mutable index = 0
    member public this.Next() : string =
        if hold.Length > index
        then 
            index <- index + 1
            hold.[index - 1]
        else
            let mutable st = stdin.ReadLine()
            while st = "" do st <- stdin.ReadLine()
            hold <- st.Split(' ')
            if hold.Length = 0
            then
                this.Next()
            else
                index <- 1
                hold.[index - 1]
            
    member public this.NextI32() : int =
        this.Next() |> int

    member public this.ArrayI32 n : int list =
        [for _ in 0 .. n - 1 -> this.NextI32()]

    member public this.NextI64() : int64 =
        this.Next() |> int64
    
    member public this.ArrayI64 n : int64 list =
        [for _ in 0 .. n - 1 -> this.NextI64()]

    member public this.NextF32() =
        this.Next() |> float32
    
    member public this.ArrayF32 n : float32 list=
        [for _ in 0 .. n - 1 -> this.NextF32()]

    member public this.NextF64() =
        this.Next() |> double
    
    member public this.ArrayF64 n : double list =
        [for _ in 0 .. n - 1 -> this.NextF64()]

    member public __.Tuple2List n (f : int -> 'a list) =
        [for _ in 0 .. n - 1 -> f 2 |> fun x -> x.[0], x.[1]]
    
    member public __.Tuple3List n (f : int -> 'a list) =
        [for _ in 0 .. n - 1 -> f 3 |> fun x -> x.[0], x.[1], x.[2]]

end

[<EntryPoint>]
let main _ =
    // hypo = float -> float -> float -> float -> float
    let hypo (x1 : float) (y1 : float) (x2: float) (y2: float) : float = ((abs x1 - x2) ** 2.) + ((abs y1 - y2)**2.) |> sqrt
    // maxpoint = (float * float) List -> float -> float
    let rec maxpoint (list : (float * float) List) (m : float) =
        match list with
        | [(_, _)] -> m
        | (x, y)::xs -> 
            List.fold (fun acc (x2, y2) -> hypo x y x2 y2 |> max acc) 0. xs
            |> max m
            |> maxpoint xs
        | _ -> 0.0
    
    let sc = Scanner()
    let n = sc.NextI32()
    let p = sc.Tuple2List n sc.ArrayF64
    maxpoint p 0. |> printfn "%f"
    0