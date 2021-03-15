module Funcons.Core.Manual (
    Funcons.Core.Manual.library
    , module Funcons.Core.Computations.Normal.GeneratingBuiltin
    , module Funcons.Core.Computations.AbnormalBuiltin
    , module Funcons.Core.Values.Composite.SetsBuiltin 
    , module Funcons.Core.Values.Composite.MultisetsBuiltin 
--    , module Funcons.Core.Values.Composite.ListsBuiltin 
    , module Funcons.Core.Values.Composite.MapsBuiltin 
    , module Funcons.Core.Values.Composite.GraphsBuiltin 
--    , module Funcons.Core.Values.Composite.VectorsBuiltin
    , module Funcons.Core.Values.Composite.DatatypesBuiltin 
    , module Funcons.Core.Values.Primitive.StringsBuiltin 
    , module Funcons.Core.Values.Primitive.BitsBuiltin 
    , module Funcons.Core.Values.Primitive.IntegersBuiltin 
    , module Funcons.Core.Values.Primitive.FloatsBuiltin 
    , module Funcons.Core.Values.Primitive.Atoms 
    , module Funcons.Core.Values.Primitive.BoolBuiltin
    , module Funcons.Core.Values.Primitive.CharactersBuiltin 
--    , module Funcons.Core.Values.Composite.TuplesBuiltin 
    , module Funcons.Core.Values.TypesBuiltin
    , module Funcons.Core.Computations.TypesBuiltin
    )where
import Funcons.EDSL

import Funcons.Core.Values.Composite.SetsBuiltin hiding (library)
import Funcons.Core.Values.Composite.MultisetsBuiltin hiding (library)
--import Funcons.Core.Values.Composite.ListsBuiltin hiding (library)
import Funcons.Core.Values.Composite.MapsBuiltin hiding (library)
import Funcons.Core.Values.Composite.GraphsBuiltin hiding (library)
--import Funcons.Core.Values.Composite.VectorsBuiltin hiding (library)
import Funcons.Core.Values.Composite.DatatypesBuiltin hiding (library)
import Funcons.Core.Values.Primitive.StringsBuiltin hiding (library)
import Funcons.Core.Values.Primitive.BitsBuiltin hiding (library)
import Funcons.Core.Values.Primitive.IntegersBuiltin hiding (library)
import Funcons.Core.Values.Primitive.FloatsBuiltin hiding (library)
import Funcons.Core.Values.Primitive.Atoms hiding (library)
import Funcons.Core.Values.Primitive.BoolBuiltin hiding (library)
import Funcons.Core.Values.Primitive.CharactersBuiltin hiding (library)
import Funcons.Core.Values.TypesBuiltin hiding (library)
import Funcons.Core.Computations.TypesBuiltin hiding (library)
--import Funcons.Core.Values.Composite.TuplesBuiltin hiding (library)

import qualified Funcons.Core.Computations.Normal.GeneratingBuiltin
import qualified Funcons.Core.Computations.AbnormalBuiltin
import qualified Funcons.Core.Values.Composite.SetsBuiltin
import qualified Funcons.Core.Values.Composite.MultisetsBuiltin
--import qualified Funcons.Core.Values.Composite.ListsBuiltin
import qualified Funcons.Core.Values.Composite.MapsBuiltin
import qualified Funcons.Core.Values.Composite.GraphsBuiltin
--import qualified Funcons.Core.Values.Composite.VectorsBuiltin
import qualified Funcons.Core.Values.Composite.DatatypesBuiltin
import qualified Funcons.Core.Values.Primitive.StringsBuiltin
import qualified Funcons.Core.Values.Primitive.BitsBuiltin
import qualified Funcons.Core.Values.Primitive.IntegersBuiltin
import qualified Funcons.Core.Values.Primitive.FloatsBuiltin
import qualified Funcons.Core.Values.Primitive.Atoms
import qualified Funcons.Core.Values.Primitive.BoolBuiltin
import qualified Funcons.Core.Values.Primitive.CharactersBuiltin
--import qualified Funcons.Core.Values.Composite.TuplesBuiltin
import qualified Funcons.Core.Values.TypesBuiltin
import qualified Funcons.Core.Computations.TypesBuiltin

library = libUnions
    [
      Funcons.Core.Computations.Normal.GeneratingBuiltin.library
--    , Funcons.Core.Computations.AbnormalBuiltin.library
    , Funcons.Core.Values.Composite.SetsBuiltin.library
    , Funcons.Core.Values.Composite.MultisetsBuiltin.library
--    , Funcons.Core.Values.Composite.ListsBuiltin.library
    , Funcons.Core.Values.Composite.MapsBuiltin.library
    , Funcons.Core.Values.Composite.GraphsBuiltin.library
--    , Funcons.Core.Values.Composite.VectorsBuiltin.library
    , Funcons.Core.Values.Composite.DatatypesBuiltin.library
    , Funcons.Core.Values.Primitive.StringsBuiltin.library
    , Funcons.Core.Values.Primitive.BitsBuiltin.library
    , Funcons.Core.Values.Primitive.IntegersBuiltin.library
    , Funcons.Core.Values.Primitive.FloatsBuiltin.library
    , Funcons.Core.Values.Primitive.Atoms.library
    , Funcons.Core.Values.Primitive.BoolBuiltin.library
    , Funcons.Core.Values.Primitive.CharactersBuiltin.library
--    , Funcons.Core.Values.Composite.TuplesBuiltin.library
    , Funcons.Core.Values.TypesBuiltin.library
    , Funcons.Core.Computations.TypesBuiltin.library
    ]
