-- test/Spec.hs
import Test.HUnit
import ASTTests (astTests)
import FormatTests (formatTests)

main :: IO ()
main = do
    putStrLn "\nRunning AST Tests..."
    _ <- runTestTT astTests
    putStrLn "\nRunning Formatter Tests..."
    _ <- runTestTT formatTests
    return ()
