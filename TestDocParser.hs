import DocParser
import Doc
import System.Exit (exitSuccess, exitFailure)


testDoc = "  first test.  \n   \n second \n test ? third test!\n\n \n"

expectedDoc = Doc [
    Par [Sen [Word "first", Word "test"] Period],
    Par [Sen [Word "second", Word "test"] Question,
         Sen [Word "third", Word "test"] Exclamation]]
          

tests = [parseDoc testDoc == Right expectedDoc,
         parseDoc ""        == Right (Doc [])]


runTests tests = do
    if (and tests)
        then exitSuccess
        else do
            print tests
            exitFailure

main = do
    runTests tests
    return ()
