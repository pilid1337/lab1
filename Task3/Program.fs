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
    | 0 -> 1., 1.
    | 1 -> aReal, aImag
    | _ -> mulComp (aReal,aImag) (powComp (aReal,aImag) (power - 1))

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
                printf "Результат: %A" (powComp (aReal, aImag) power)
                0
        else
            printf "Введите вещественную часть второго числа: "
            let bReal  = float (Console.ReadLine())

            printf "Введите мнимую часть второго числа: "
            let bImag  = float (Console.ReadLine())

            match operation with
            | 1 -> printf "Сумма: %A" (sumComp (aReal, aImag) (bReal, bImag))
            | 2 -> printf "Разность: %A" (subComp (aReal, aImag) (bReal, bImag))
            | 3 -> printf "Произведение: %A" (mulComp (aReal, aImag) (bReal, bImag))
            | 4 -> printf "Частное: %A" (divComp (aReal, aImag) (bReal, bImag))
            | _ -> printfn "Непредвиденная ситуация"

            0