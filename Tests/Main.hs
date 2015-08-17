

import Test.Tasty
import Tests.AttoScoped
import Tests.Command
import Tests.Condition
import Tests.Maker
import Tests.Modifier
import Tests.ScopeType
import Tests.Trait

main = defaultMain $ testGroup "Tests" [ commandUnitTests, conditionTests, parseTests, makerTests, modifierTests, scopeTests, scopeTypeTests, traitTests ]
