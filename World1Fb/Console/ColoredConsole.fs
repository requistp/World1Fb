module ColoredConsole

module Console =
//http://www.fssnip.net/7Vy/title/Supersimple-thread-safe-colored-console-output

// go here when I expand:
// https://blog.vbfox.net/2016/10/17/more-fsharp-colors-in-terminal.html
    open System

    let private log =
        let lockObj = obj()
        fun color (s:char) ->
            lock lockObj (fun _ ->
                //Console.BackgroundColor <- ConsoleColor.DarkYellow
                Console.ForegroundColor <- color
                Console.Write(s)
                //printfn "%s" s
                Console.ResetColor())

    let DrawMagenta = log ConsoleColor.Magenta
    let DrawGreen = log ConsoleColor.Green
    let DrawCyan = log ConsoleColor.Cyan
    let DrawYellow = log ConsoleColor.Yellow
    let DrawRed = log ConsoleColor.Red
    let DrawBlack = log ConsoleColor.Black
    let DrawBlue = log ConsoleColor.Blue
    let DrawDarkBlue = log ConsoleColor.DarkBlue
    let DrawDarkCyan = log ConsoleColor.DarkCyan
    let DrawDarkGray = log ConsoleColor.DarkGray
    let DrawDarkGreen = log ConsoleColor.DarkGreen
    let DrawDarkMagenta = log ConsoleColor.DarkMagenta
    let DrawDarkRed = log ConsoleColor.DarkRed
    let DrawDarkYellow = log ConsoleColor.DarkYellow
    let DrawGray = log ConsoleColor.Gray
    let DrawWhite = log ConsoleColor.White

