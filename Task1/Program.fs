open System

[<EntryPoint>]
let main args=
    printf "Введите кол-во чисел: "

    let n = int (Console.ReadLine())

    if n <= 0 then
        printfn "Введено неправильное количество"
        1 // Ошибка ввода
    else
        printfn "Введите числа:"

        let input = [
            for i: int in [1..n] do
                yield float (Console.ReadLine())
        ]

        let output = input |> List.map (fun x -> x / 2.)

        printfn "%A" output
        0