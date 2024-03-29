type Scanner() = class
    let mutable hold = [||]
    let mutable index = 0
    member public this.Next() =
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
            
    member public this.NextI32() =
        this.Next() |> int

    member public this.ArrayI32(n: int) =
        [for _ in 0 .. n - 1 -> this.NextI32()]

    member public this.NextI64() =
        this.Next() |> int64
    
    member public this.ArrayI64(n: int) =
        [for _ in 0 .. n - 1 -> this.NextI64()]

    member public this.NextF32() =
        this.Next() |> float32
    
    member public this.ArrayF32(n: int) =
        [for _ in 0 .. n - 1 -> this.NextF32()]

    member public this.NextF64() =
        this.Next() |> double
    
    member public this.ArrayF64(n: int) =
        [for _ in 0 .. n - 1 -> this.NextF64()]
end

[<EntryPoint>]
let main _ =
    // answer = int -> int -> int
    let rec answer ind en eqnum count = 
        match ind with
        | a when a > en -> count
        | x -> List.fold(fun acc p -> acc + if (fun a b -> a >= 0 && a <= b) (eqnum - (p + x)) en then 1 else 0) 0 [0..en] + count
            |> answer (ind + 1) en eqnum 
    let sc = Scanner()
    let k = sc.NextI32()
    let s = sc.NextI32()
    answer 0 k s 0 |> printfn "%i"
    0
