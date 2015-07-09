

import Test.Tasty
import Tests.Command
import Tests.AttoScoped
import Tests.Maker

main = defaultMain $ testGroup "Tests" [ commandUnitTests, parseTests, makerTests ]
