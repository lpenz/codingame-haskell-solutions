import System.IO
import Control.Monad

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE
    input_line <- getLine
    let n = read input_line :: Int -- the number of points used to draw the surface of Mars.
    replicateM_ n $ do
        input_line <- getLine
        let input = words input_line
        let land_x = read (head input) :: Int -- X coordinate of a surface point. (0 to 6999)
        let land_y = read (input!!1) :: Int -- Y coordinate of a surface point. By linking all the points together in a sequential fashion, you form the surface of Mars.
        return ()
    loop

loop :: IO ()
loop = do
    input_line <- getLine
    let input = words input_line
    let x = read (head input) :: Int
    let y = read (input!!1) :: Int
    let hs = read (input!!2) :: Int -- the horizontal speed (in m/s), can be negative.
    let vs = read (input!!3) :: Int -- the vertical speed (in m/s), can be negative.
    let f = read (input!!4) :: Int -- the quantity of remaining fuel in liters.
    let r = read (input!!5) :: Int -- the rotation angle in degrees (-90 to 90).
    let p = read (input!!6) :: Int -- the thrust power (0 to 4).
    let signal x = if x >= 0 then 1 else -1
    let rotate = if abs r > 15 then signal r * (-15) else -r
    let power = if vs <= -40
        then min 4 (p + 1)
        else max 0 (p - 1)
    let ans = show rotate ++ " " ++ show power
    hPrint stderr ans
    putStrLn ans
    loop
