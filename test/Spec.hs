-- test/Spec.hs
import Test.HUnit
import ASTTests (astTests)

main :: IO ()
main = do
    putStrLn "Running AST Tests..."
    _ <- runTestTT astTests
    return ()
