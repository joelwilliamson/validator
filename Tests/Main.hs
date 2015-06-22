

import Test.Tasty
import Tests.Command

main = defaultMain $ testGroup "Tests" [commandUnitTests]
