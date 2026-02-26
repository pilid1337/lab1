open System

let sumComp (aReal:float, aImag:float) (bReal:float, bImag:float) : float*float = 
    aReal+bReal, aImag+bImag

let subComp (aReal:float, aImag:float) (bReal:float, bImag:float) : float*float = 
    aReal-bReal, aImag-bImag

let mulComp (aReal:float, aImag:float) (bReal:float, bImag:float) : float*float = 
    aReal*bReal - aImag*bImag, aImag*bReal+bImag*aReal

let divComp (aReal:float, aImag:float) (bReal:float, bImag:float) : (float*float) option = 
    // Домножаем делитель и знаменатель, чтобы получить квадрат разности в знаменателе
    let divider = bReal * bReal + bImag * bImag

    if divider = 0 then
        None
    else
        Some ((aReal*bReal + aImag*bImag) / divider, (aImag*bReal-bImag*aReal) / divider)

let rec powComp (aReal:float, aImag:float) power:int :float*float =
    match power with
    | 0 -> 1., 0.
    | 1 -> aReal, aImag
    | _ -> mulComp (aReal,aImag) (powComp (aReal,aImag) (power - 1))

let printComp (aReal:float, aImag:float) = 
    printfn "Результат: %f + %fi" aReal aImag

[<EntryPoint>]
let main args =   
    printf "Введите операцию (1 - сложение, 2 - вычитание, 3 - умножение, 4 - деление, 5 - возведение в степень): "
    let operation = int (Console.ReadLine())

    if operation < 1 || operation > 5 then
        printfn "Неверная операция"
        1
    else 
        printf "Введите вещественную часть первого числа: "
        let aReal  = float (Console.ReadLine())

        printf "Введите мнимую часть первого числа: "
        let aImag  = float (Console.ReadLine())

        if operation = 5 then
            printf "Введите степень числа: "
            let power = int (Console.ReadLine())

            if power < 0 then
                printfn "Неверно введена степень"
                1
            else
                printComp (powComp (aReal, aImag) power)
                0
        else
            printf "Введите вещественную часть второго числа: "
            let bReal  = float (Console.ReadLine())

            printf "Введите мнимую часть второго числа: "
            let bImag  = float (Console.ReadLine())

            match operation with
            | 1 -> 
                printComp (sumComp (aReal, aImag) (bReal, bImag)) 
                0
            | 2 -> 
                printComp (subComp (aReal, aImag) (bReal, bImag))
                0
            | 3 -> 
                printComp (mulComp (aReal, aImag) (bReal, bImag))
                0
            | 4 -> 
                let div = divComp (aReal, aImag) (bReal, bImag)
                if div.IsNone then
                    printfn "Деление на 0"
                    1
                else
                    printComp div.Value
                    0
            | _ -> 
                printfn "Непредвиденная ситуация"
                1