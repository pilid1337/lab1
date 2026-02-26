open System

let rec getFirstDigit x:int = 
    if x < 10 then
        x
    else 
        getFirstDigit (x / 10)

[<EntryPoint>]
let main args =   
    printf "Введите натуральное число: "
    let input  = int (Console.ReadLine())

    printfn "Чётность первой цифры: %b" (getFirstDigit input % 2 = 0)
    0