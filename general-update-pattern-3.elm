{- This example is based on: http://bl.ocks.org/mbostock/3808234
    Aside from the library itself, I think the definition of `clock` will be
    very useful for those building animated transitions on this library.
    Also, it demonstrates that you can use any rendering pipeline you like.
-}

import Char exposing (KeyCode)
import Color exposing (red, green, black)
import Graphics.Collage as C
import Graphics.Element exposing (Element, show)
import Random
import String exposing (fromChar)
import Text as T
import Time exposing (Time)
import Window

import Join exposing (Join)

-- random generators for the alphabet
bool : Random.Generator Bool
bool =
    let gen = Random.int 0 4096
    in Random.customGenerator (\seed ->
        let (x, seed') = Random.generate gen seed
        in (x > 2048, seed'))

aCode : KeyCode
aCode = Char.toCode 'a'

type alias Alphabet = List Char

{- Randomly generate a subset of the alphabet. Simply flips 26 coins. Contrast Mike Bostock's generator, which shuffles
the alphabet and then takes a sublist of random length, ensuring that empty and full alphabets occur.
-}
generateAlphabet : Random.Generator Alphabet
generateAlphabet =
    Random.customGenerator (\seed ->
        let (bs, seed') = Random.generate (Random.list 26 bool) seed
            maybeLetters = List.indexedMap (\i b -> if b then Just (Char.fromCode (aCode+i)) else Nothing) bs
        in (List.filterMap identity maybeLetters, seed'))

-- Signals of the alphabet and time since update

alphabet : Signal Alphabet
alphabet =
    Signal.foldp (\_ (_, seed) -> Random.generate generateAlphabet seed) ([], Random.initialSeed 42) (Time.fps 0.8)
        |> Signal.map fst

duration : Time
duration = 0.7*Time.second

{- The combination of Time.since, Time.fpsWhen, and summing with a reset to 0
   creates a sort of triangular wave that drives the transitions.
-}
clock : Signal Time
clock =
    let boolClock = Time.since duration alphabet
        deltas = Time.fpsWhen 30 boolClock
        sum d t = if d == 0 then 0 else d+t
    in Signal.foldp sum 0 deltas

-- Join

type alias Datum = {char : Char, pos : Int}

step : Alphabet -> Join Datum -> Join Datum
step abc oldJoin =
    Join.updateBy .char (List.map (\c -> Datum c 0) abc) oldJoin
        |> Join.edit (List.sortBy (snd >> .char) >> List.indexedMap (\i (d1, d2) -> (d1, {d2| pos <- i})))

data : Signal (Join Datum)
data = Signal.foldp step Join.empty alphabet

-- Render
toText : Char -> T.Text
toText =
    fromChar >> T.fromString >> T.monospace >> T.height 48 >> T.bold

lerp : Float -> Float -> Float -> Float
lerp a b t = a + (b-a)*t

dx = 40
dy = 60

scene : (Int, Int) -> Time -> Join Datum -> Element
scene (w,h) t {enter, update, exit} =
    let lrp a b = lerp a b (t/duration |> min 1)
        en {char, pos} = toText char |> T.color green |> C.text |> C.move (toFloat pos*dx, lrp dy 0) |> C.alpha (lrp 0 1)
        ud (old, new) = toText new.char |> C.text |> C.moveX (dx*(lrp (toFloat old.pos) (toFloat new.pos)))
        ex {char, pos} = toText char |> T.color red |> C.text |> C.move (toFloat pos*dx, lrp 0 -dy) |> C.alpha (lrp 1 0)
    in C.collage w h [C.group (List.map en enter ++ List.map ud update ++ List.map ex exit) |> C.moveX (toFloat <| -w//2 + dx)]

main = Signal.map3 scene Window.dimensions clock data
