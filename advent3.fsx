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

console.Error.Add raise

let setAsync x y c m =
  async { console.Post(x, y, c, m) }

type Vector2 =
  { x : float; y : float }
  static member (+) ({ x = x1; y = y1 }, { x = x2; y = y2 }) =
    { x = x1 + x2; y = y1 + y2 }
  static member (-) ({ x = x1; y = y1 }, { x = x2; y = y2 }) =
    { x = x1 - x2; y = y1 - y2 }
  static member (/) ({ x = x1; y = y1 }, d) =
    { x = x1 / d; y = y1 / d }
  static member ( *) ({ x = x1; y = y1 }, s) =
    { x = x1 * s; y = y1 * s }
  static member abs { x = x1; y = y1 } =
    x1 * x1 + y1 * y1
    |> sqrt
  static member limit maximum v =
    let magnitude = Vector2.abs v
    if magnitude > maximum then
      let ratio = maximum / magnitude
      { x = v.x * ratio; y = v.y * ratio }
    else v

type AngelInfo =
  { Position : Vector2
    Velocity : Vector2
    Colour   : int }

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
            |> List.map (fun { Position = p; Colour = c } -> setAsync (int p.x) (int p.y) c '*')
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

ping.Error.Add raise

let seedx, seedy, seedc = 100, 150, 200

let randX = Random(seedx)
let randY = Random(seedy)
let randC = Random(seedc)

let randSeq (rand : Random) min' max' =
  Seq.unfold (fun () -> Some(rand.Next(min', max'), ())) ()

let xSeq  = randSeq randX xZero (width + xZero - 1)
let ySeq  = randSeq randY yZero (height + yZero - 1)
let cSeq  = randSeq randC 1 15
let vxSeq = randSeq randX -1 1
let vySeq = randSeq randY -1 1

let createAngel logic angelInfo =
  let angel =
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
  angel.Error.Add (fun e -> Console.Write e.Message)
  angel

module Logic =
  let private surrounding radius (us : AngelInfo) (others : AngelInfo list) =
    others
    |> List.filter (fun a -> Vector2.abs (a.Position - us.Position) < radius)

  let private desiredVel angels =
    angels
    |> List.fold
        (fun (v, i) angel ->
          (angel.Velocity + v, i + 1)) ({ x = 0.; y = 0.}, 0)
    |> fun (v, i) ->
        match i with
        | 0 ->
          { x = 0.; y = 0. }
        | _ ->
          { x = v.x / float i; y = v.y / float i }

  let private avoid this angels =
    let dodge v =
      { x = 1. / v.x * -1.
        y = 1. / v.y * -1. } * (List.length angels |> float)
    match angels with
    | [] | [_] -> { x = 0.; y = 0. }
    | _ ->
      angels
      |> List.map (fun angel -> angel.Position - this.Position)
      |> List.reduce (+)
      |> dodge

  let boid midpoint friendRadius dodgeRadius maxAcc maxVel this angels =
    let groupVel =
      surrounding friendRadius this angels
      |> desiredVel
      |> Vector2.limit maxVel
    let avoidCollision =
      surrounding dodgeRadius this angels
      |> avoid this
      |> Vector2.limit maxVel
    let towardsMiddle =
      midpoint - this.Position
      |> Vector2.limit maxVel
    let acceleration =
      (groupVel * 0.5 + avoidCollision * 2. + towardsMiddle)
      / 3.
    { this with Position = this.Position + this.Velocity
                Velocity = (this.Velocity + acceleration) |> Vector2.limit maxVel }

  let stationary this _ =
    { this with Velocity = { x = 0.; y = 0. } }

let midpoint =
  { x = float (width - xZero) / 2.
        + float xZero
    y = float (height - yZero) / 2.
        + float yZero }

let angels =
  Seq.zip3 (Seq.zip xSeq ySeq) (Seq.zip vxSeq vySeq) cSeq
  |> Seq.take 40
  |> Seq.map
      (fun ((px, py), (vx, vy), c) ->
       { Position = { x = float px; y = float py }
         Velocity = { x = float vx; y = float vy }
         Colour   = c })
  |> Seq.map (createAngel (Logic.boid midpoint 10. 1. 0.3 1.))
  |> Seq.append
      [(createAngel
         Logic.stationary { Position = midpoint
                            Velocity = { x = 0.; y = 0. }
                            Colour   = 15 })]
  |> Seq.toList

// Start the whole thing off
ping.Post angels

Console.ReadLine()

Console.ForegroundColor <- ConsoleColor.White
Console.CursorVisible <- true
