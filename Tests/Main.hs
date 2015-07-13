

import Test.Tasty
import Tests.AttoScoped
import Tests.Command
import Tests.Condition
import Tests.Maker
import Tests.ScopeType

main = defaultMain $ testGroup "Tests" [ commandUnitTests, conditionTests, parseTests, makerTests, scopeTypeTests ]
