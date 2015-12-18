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

let (|X|) x =
  if x < xZero then
    xZero
  elif x >= width then
    width - 1
  else
    x

let (|Y|) y =
  if y < yZero then
    yZero
  elif y >= height then
    height - 1
  else
    y

let console =
  MailboxProcessor.Start(
    fun agent ->
      let rec inner () =
        async {
          let! (X x, Y y, ConsoleColour c, m : char) = agent.Receive()
          Console.SetCursorPosition(x, y)
          Console.ForegroundColor <- c
          Console.Write (string m)
          return! inner () }
      inner ())

console.Error.Add(fun e -> printfn "%A" e.Message)

let setAsync x y c m =
  async { console.Post(x, y, c, m) }

type Vector2 =
  { x : float; y : float }
  static member (+) ({ x = x1; y = y1 }, { x = x2; y = y2 }) =
    { x = x1 + x2; y = y1 + y2 }
  static member (-) ({ x = x1; y = y1 }, { x = x2; y = y2 }) =
    { x = x1 - x2; y = y1 - y2 }
  static member Abs { x = x1; y = y1 } =
    x1 * x1 + y1 * y1
    |> sqrt

type AngelInfo =
  { Position : Vector2
    Velocity : Vector2 }

type AngelMessage =
  | Init of AsyncReplyChannel<AngelInfo>
  | Next of AngelInfo list * AsyncReplyChannel<AngelInfo>

let ping =
  MailboxProcessor.Start(
    fun agent ->
      let rec inner (angels : MailboxProcessor<AngelMessage> list) infos =
        async {
          // Ask the angels where they will be next
          let! newInfos =
            angels
            |> List.map (fun angel -> angel.PostAndAsyncReply (fun r -> Next(infos, r)))
            |> Async.Parallel

          let newInfos = newInfos |> List.ofArray

          // Erase old locations
          do!
            infos
            |> List.map (fun { Position = p } -> p)
            |> List.map (fun p -> setAsync (int p.x) (int p.y) 0 ' ')
            |> Async.Parallel
            |> Async.Ignore

          // Draw new locations
          do!
            newInfos
            |> List.map (fun { Position = p } -> p)
            |> List.map (fun p -> setAsync (int p.x) (int p.y) 15 '*')
            |> Async.Parallel
            |> Async.Ignore

          do! Async.Sleep 100
          return! inner angels newInfos
        }
      let init () =
        async {
          // Wait for angels to be passed in
          let! (msg : MailboxProcessor<AngelMessage> list) = agent.Receive()

          let! infos =
            msg
            |> List.map (fun angel -> angel.PostAndAsyncReply Init)
            |> Async.Parallel

          return! inner msg (infos |> List.ofArray)
        }
      init ()
    )

let seedx, seedy, seedc = 100, 150, 200

let randX = Random(seedx)
let randY = Random(seedy)
let randC = Random(seedc)

let randSeq (rand : Random) min' max' =
  Seq.unfold (fun () -> Some(rand.Next(min', max'), ())) ()

let xSeq  = randSeq randX xZero (width + xZero - 1)
let ySeq  = randSeq randY yZero (height + yZero - 1)
let vxSeq = randSeq randX -5 5
let vySeq = randSeq randY -5 5

let createAngel logic angelInfo =
  MailboxProcessor.Start(
    fun agent ->
      let rec inner currentInfo =
        async {
          let! msg = agent.Receive()
          return!
            match msg with
            | Init r ->
              r.Reply currentInfo
              inner currentInfo
            | Next (infos, r) ->
              let newInfo = logic currentInfo infos
              r.Reply newInfo
              inner newInfo
        }
      inner angelInfo)

let angels =
  Seq.zip (Seq.zip xSeq ySeq) (Seq.zip vxSeq vySeq)
  |> Seq.take 10
  |> Seq.map
      (fun ((px, py), (vx, vy)) ->
        { Position = { x = float px; y = float py }; Velocity = { x = float vx; y = float py }})
  |> Seq.map (createAngel (fun c _ -> { c with Position = c.Position + c.Velocity }))
  |> Seq.toList

// Start the whole thing off
ping.Post angels

Console.ReadLine()

Console.ForegroundColor <- ConsoleColor.White
Console.CursorVisible <- true
