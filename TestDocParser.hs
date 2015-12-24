import DocParser
import System.Exit (exitSuccess, exitFailure)


testDoc      = "\n \n  First   sentence?  \n \n Second    sentence. \n \n  Third   sentence !  \n  \n "
referenceDoc = "First sentence?\n\nSecond sentence.\n\nThird sentence!"


tests :: [Bool]
tests = [parseDoc testDoc == parseDoc referenceDoc]

runTests :: [Bool] -> IO Int
runTests tests = do
    if (and tests)
        then exitSuccess
        else do
            print tests
            exitFailure


main :: IO ()
main = do
    runTests tests
    return ()
