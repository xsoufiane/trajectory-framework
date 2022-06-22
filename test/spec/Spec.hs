import Test.Tasty

import qualified Spec.Annotation.AnnotationSpec as AnnotationSpec
import qualified Spec.Annotation.SemanticAnnotationSpec as SemanticAnnotationSpec 

-----------------------------------

main :: IO ()
main = defaultMain $ testGroup "Trajectory Algebra Spec" [ AnnotationSpec.spec, SemanticAnnotationSpec.spec ]
