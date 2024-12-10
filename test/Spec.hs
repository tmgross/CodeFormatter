-- test/Spec.hs
import Test.HUnit
import ASTTests (astTests)

main :: IO ()
main = do
    putStrLn "\nRunning AST Tests..."
    _ <- runTestTT astTests
    return ()
