

import Test.Tasty
import Tests.Command
import Tests.AttoScoped

main = defaultMain $ testGroup "Tests" [ parseTests, commandUnitTests]
