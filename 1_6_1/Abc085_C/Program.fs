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
    let sc = Scanner()
    let n = sc.NextI32()
    let y = sc.NextI32()

    let res = seq {
        for i in 0 .. n do
            for j in 0 .. n - i do
                let k = n - i - j
                if i * 10000 + j * 5000 + k * 1000 = y then yield (i, j, k)
    }
    if Seq.exists(fun (t, g, s) -> t >= 0 && g >= 0 && s >= 0) res then
        let a, b, c = Seq.head res
        printfn "%d %d %d" a b c
    else
        printfn "-1 -1 -1"
    0
