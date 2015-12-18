open System

Console.Clear()
Console.CursorVisible <- false

let width  = Console.WindowWidth
let height = Console.WindowHeight

let xZero  = Console.WindowLeft
let yZero  = Console.WindowTop

let (|ConsoleColour|) i =
  if i <= 0 then
    ConsoleColor.Black
  elif i >= 15 then
    ConsoleColor.White
  else
    enum i

let console =
  MailboxProcessor.Start(
    fun agent ->
      let rec inner () =
        async {
          let! (x, y, ConsoleColour c, m : char) = agent.Receive()
          Console.SetCursorPosition(x, y)
          Console.ForegroundColor <- c
          Console.Write (string m)
          return! inner () }
      inner ())

console.Error.Add(fun e -> printfn "%A" e.Message)

let seedx, seedy, seedc = 100, 150, 200

let randX = Random(seedx)
let randY = Random(seedy)
let randC = Random(seedc)

let randSeq (rand : Random) min' max' =
  Seq.unfold (fun () -> Some(rand.Next(min', max'), ())) ()

let xSeq = randSeq randX xZero (width + xZero - 1)
let ySeq = randSeq randY yZero (height + yZero - 1)
let cSeq = randSeq randC 0 15

let setAsync x y c m =
  async { console.Post(x, y, c, m) }

let angels =
  Seq.zip3 xSeq ySeq cSeq
  |> Seq.take 100
  |> Seq.mapi (fun i (x, y, c) ->
                  async {
                    do! Async.Sleep (x * 50)
                    do! setAsync x y c '*'
                  })
  |> Async.Parallel
  |> Async.RunSynchronously

Console.ReadLine()

Console.ForegroundColor <- ConsoleColor.White
Console.CursorVisible <- true
