-- Echoes stdin lines in upper case until an empty line, using the
-- home-made IO monad from MyIO.
import MyIO (convert)
import MyEcho (myEcho)

main :: IO ()
main = convert myEcho
