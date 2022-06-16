import Test.Tasty

import qualified AnnotationSpec

-----------------------------------

main :: IO ()
main = defaultMain $ testGroup "Trajectory Algebra Spec"
    [ AnnotationSpec.spec ]

