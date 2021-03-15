module Funcons.Core.Library (
    funcons, entities, types,
   module Funcons.Core.Computations.Normal.Interacting.Interacting,
   module Funcons.Core.Computations.Normal.Flowing.Flowing,
   module Funcons.Core.Computations.Normal.Storing.Storing,
   module Funcons.Core.Computations.Normal.Linking.Linking,
   module Funcons.Core.Computations.Normal.Giving.Giving,
   module Funcons.Core.Computations.Normal.Generating.Generating,
   module Funcons.Core.Computations.Normal.Binding.Binding,
   module Funcons.Core.Computations.Abnormal.Abrupting.Abrupting,
   module Funcons.Core.Computations.Abnormal.Returning.Returning,
   module Funcons.Core.Computations.Abnormal.Controlling.Controlling,
   module Funcons.Core.Computations.Abnormal.Throwing.Throwing,
   module Funcons.Core.Computations.Abnormal.Failing.Failing,
   module Funcons.Core.Computations.Abnormal.Sticking,
   module Funcons.Core.Computations.Abnormal.Breaking.Breaking,
   module Funcons.Core.Computations.Abnormal.Continuing.Continuing,
   module Funcons.Core.Values.Composite.ASTs.ASTs,
   module Funcons.Core.Values.Composite.Records.Records,
   module Funcons.Core.Values.Composite.Strings.Strings,
   module Funcons.Core.Values.Composite.References.References,
   module Funcons.Core.Values.Composite.Graphs.Graphs,
   module Funcons.Core.Values.Composite.Vectors.Vectors,
   module Funcons.Core.Values.Composite.Variants.Variants,
   module Funcons.Core.Values.Composite.Trees.Trees,
   module Funcons.Core.Values.Composite.Sequences.Sequences,
   module Funcons.Core.Values.Composite.Lists.Lists,
   module Funcons.Core.Values.Composite.Classes.Classes,
   module Funcons.Core.Values.Composite.Objects.Objects,
   module Funcons.Core.Values.Composite.Datatypes.Datatypes,
   module Funcons.Core.Values.Composite.Tuples.Tuples,
   module Funcons.Core.Values.Composite.Bits.Bits,
   module Funcons.Core.Values.Primitive.Floats.Floats,
   module Funcons.Core.Values.Primitive.Booleans.Booleans,
   module Funcons.Core.Values.Primitive.Null.Null,
   module Funcons.Core.Values.Primitive.Characters.Characters,
   module Funcons.Core.Values.Primitive.Integers.Integers,
   module Funcons.Core.Values.ValueTypes.ValueTypes,
   module Funcons.Core.Values.Abstraction.Thunks.Thunks,
   module Funcons.Core.Values.Abstraction.Patterns.Patterns,
   module Funcons.Core.Values.Abstraction.Functions.Functions,
   module Funcons.Core.Values.Abstraction.Generic.Generic,
    ) where 
import Funcons.EDSL
import Funcons.Core.Computations.Normal.Interacting.Interacting hiding (funcons,types,entities)
import qualified Funcons.Core.Computations.Normal.Interacting.Interacting
import Funcons.Core.Computations.Normal.Flowing.Flowing hiding (funcons,types,entities)
import qualified Funcons.Core.Computations.Normal.Flowing.Flowing
import Funcons.Core.Computations.Normal.Storing.Storing hiding (funcons,types,entities)
import qualified Funcons.Core.Computations.Normal.Storing.Storing
import Funcons.Core.Computations.Normal.Linking.Linking hiding (funcons,types,entities)
import qualified Funcons.Core.Computations.Normal.Linking.Linking
import Funcons.Core.Computations.Normal.Giving.Giving hiding (funcons,types,entities)
import qualified Funcons.Core.Computations.Normal.Giving.Giving
import Funcons.Core.Computations.Normal.Generating.Generating hiding (funcons,types,entities)
import qualified Funcons.Core.Computations.Normal.Generating.Generating
import Funcons.Core.Computations.Normal.Binding.Binding hiding (funcons,types,entities)
import qualified Funcons.Core.Computations.Normal.Binding.Binding
import Funcons.Core.Computations.Abnormal.Abrupting.Abrupting hiding (funcons,types,entities)
import qualified Funcons.Core.Computations.Abnormal.Abrupting.Abrupting
import Funcons.Core.Computations.Abnormal.Returning.Returning hiding (funcons,types,entities)
import qualified Funcons.Core.Computations.Abnormal.Returning.Returning
import Funcons.Core.Computations.Abnormal.Controlling.Controlling hiding (funcons,types,entities)
import qualified Funcons.Core.Computations.Abnormal.Controlling.Controlling
import Funcons.Core.Computations.Abnormal.Throwing.Throwing hiding (funcons,types,entities)
import qualified Funcons.Core.Computations.Abnormal.Throwing.Throwing
import Funcons.Core.Computations.Abnormal.Failing.Failing hiding (funcons,types,entities)
import qualified Funcons.Core.Computations.Abnormal.Failing.Failing
import Funcons.Core.Computations.Abnormal.Sticking hiding (funcons,types,entities)
import qualified Funcons.Core.Computations.Abnormal.Sticking
import Funcons.Core.Computations.Abnormal.Breaking.Breaking hiding (funcons,types,entities)
import qualified Funcons.Core.Computations.Abnormal.Breaking.Breaking
import Funcons.Core.Computations.Abnormal.Continuing.Continuing hiding (funcons,types,entities)
import qualified Funcons.Core.Computations.Abnormal.Continuing.Continuing
import Funcons.Core.Values.Composite.ASTs.ASTs hiding (funcons,types,entities)
import qualified Funcons.Core.Values.Composite.ASTs.ASTs
import Funcons.Core.Values.Composite.Records.Records hiding (funcons,types,entities)
import qualified Funcons.Core.Values.Composite.Records.Records
import Funcons.Core.Values.Composite.Strings.Strings hiding (funcons,types,entities)
import qualified Funcons.Core.Values.Composite.Strings.Strings
import Funcons.Core.Values.Composite.References.References hiding (funcons,types,entities)
import qualified Funcons.Core.Values.Composite.References.References
import Funcons.Core.Values.Composite.Graphs.Graphs hiding (funcons,types,entities)
import qualified Funcons.Core.Values.Composite.Graphs.Graphs
import Funcons.Core.Values.Composite.Vectors.Vectors hiding (funcons,types,entities)
import qualified Funcons.Core.Values.Composite.Vectors.Vectors
import Funcons.Core.Values.Composite.Variants.Variants hiding (funcons,types,entities)
import qualified Funcons.Core.Values.Composite.Variants.Variants
import Funcons.Core.Values.Composite.Trees.Trees hiding (funcons,types,entities)
import qualified Funcons.Core.Values.Composite.Trees.Trees
import Funcons.Core.Values.Composite.Sequences.Sequences hiding (funcons,types,entities)
import qualified Funcons.Core.Values.Composite.Sequences.Sequences
import Funcons.Core.Values.Composite.Lists.Lists hiding (funcons,types,entities)
import qualified Funcons.Core.Values.Composite.Lists.Lists
import Funcons.Core.Values.Composite.Classes.Classes hiding (funcons,types,entities)
import qualified Funcons.Core.Values.Composite.Classes.Classes
import Funcons.Core.Values.Composite.Objects.Objects hiding (funcons,types,entities)
import qualified Funcons.Core.Values.Composite.Objects.Objects
import Funcons.Core.Values.Composite.Datatypes.Datatypes hiding (funcons,types,entities)
import qualified Funcons.Core.Values.Composite.Datatypes.Datatypes
import Funcons.Core.Values.Composite.Tuples.Tuples hiding (funcons,types,entities)
import qualified Funcons.Core.Values.Composite.Tuples.Tuples
import Funcons.Core.Values.Composite.Bits.Bits hiding (funcons,types,entities)
import qualified Funcons.Core.Values.Composite.Bits.Bits
import Funcons.Core.Values.Primitive.Floats.Floats hiding (funcons,types,entities)
import qualified Funcons.Core.Values.Primitive.Floats.Floats
import Funcons.Core.Values.Primitive.Booleans.Booleans hiding (funcons,types,entities)
import qualified Funcons.Core.Values.Primitive.Booleans.Booleans
import Funcons.Core.Values.Primitive.Null.Null hiding (funcons,types,entities)
import qualified Funcons.Core.Values.Primitive.Null.Null
import Funcons.Core.Values.Primitive.Characters.Characters hiding (funcons,types,entities)
import qualified Funcons.Core.Values.Primitive.Characters.Characters
import Funcons.Core.Values.Primitive.Integers.Integers hiding (funcons,types,entities)
import qualified Funcons.Core.Values.Primitive.Integers.Integers
import Funcons.Core.Values.ValueTypes.ValueTypes hiding (funcons,types,entities)
import qualified Funcons.Core.Values.ValueTypes.ValueTypes
import Funcons.Core.Values.Abstraction.Thunks.Thunks hiding (funcons,types,entities)
import qualified Funcons.Core.Values.Abstraction.Thunks.Thunks
import Funcons.Core.Values.Abstraction.Patterns.Patterns hiding (funcons,types,entities)
import qualified Funcons.Core.Values.Abstraction.Patterns.Patterns
import Funcons.Core.Values.Abstraction.Functions.Functions hiding (funcons,types,entities)
import qualified Funcons.Core.Values.Abstraction.Functions.Functions
import Funcons.Core.Values.Abstraction.Generic.Generic hiding (funcons,types,entities)
import qualified Funcons.Core.Values.Abstraction.Generic.Generic
funcons = libUnions
    [
     Funcons.Core.Computations.Normal.Interacting.Interacting.funcons
    , Funcons.Core.Computations.Normal.Flowing.Flowing.funcons
    , Funcons.Core.Computations.Normal.Storing.Storing.funcons
    , Funcons.Core.Computations.Normal.Linking.Linking.funcons
    , Funcons.Core.Computations.Normal.Giving.Giving.funcons
    , Funcons.Core.Computations.Normal.Generating.Generating.funcons
    , Funcons.Core.Computations.Normal.Binding.Binding.funcons
    , Funcons.Core.Computations.Abnormal.Abrupting.Abrupting.funcons
    , Funcons.Core.Computations.Abnormal.Returning.Returning.funcons
    , Funcons.Core.Computations.Abnormal.Controlling.Controlling.funcons
    , Funcons.Core.Computations.Abnormal.Throwing.Throwing.funcons
    , Funcons.Core.Computations.Abnormal.Failing.Failing.funcons
    , Funcons.Core.Computations.Abnormal.Sticking.funcons
    , Funcons.Core.Computations.Abnormal.Breaking.Breaking.funcons
    , Funcons.Core.Computations.Abnormal.Continuing.Continuing.funcons
    , Funcons.Core.Values.Composite.ASTs.ASTs.funcons
    , Funcons.Core.Values.Composite.Records.Records.funcons
    , Funcons.Core.Values.Composite.Strings.Strings.funcons
    , Funcons.Core.Values.Composite.References.References.funcons
    , Funcons.Core.Values.Composite.Graphs.Graphs.funcons
    , Funcons.Core.Values.Composite.Vectors.Vectors.funcons
    , Funcons.Core.Values.Composite.Variants.Variants.funcons
    , Funcons.Core.Values.Composite.Trees.Trees.funcons
    , Funcons.Core.Values.Composite.Sequences.Sequences.funcons
    , Funcons.Core.Values.Composite.Lists.Lists.funcons
    , Funcons.Core.Values.Composite.Classes.Classes.funcons
    , Funcons.Core.Values.Composite.Objects.Objects.funcons
    , Funcons.Core.Values.Composite.Datatypes.Datatypes.funcons
    , Funcons.Core.Values.Composite.Tuples.Tuples.funcons
    , Funcons.Core.Values.Composite.Bits.Bits.funcons
    , Funcons.Core.Values.Primitive.Floats.Floats.funcons
    , Funcons.Core.Values.Primitive.Booleans.Booleans.funcons
    , Funcons.Core.Values.Primitive.Null.Null.funcons
    , Funcons.Core.Values.Primitive.Characters.Characters.funcons
    , Funcons.Core.Values.Primitive.Integers.Integers.funcons
    , Funcons.Core.Values.ValueTypes.ValueTypes.funcons
    , Funcons.Core.Values.Abstraction.Thunks.Thunks.funcons
    , Funcons.Core.Values.Abstraction.Patterns.Patterns.funcons
    , Funcons.Core.Values.Abstraction.Functions.Functions.funcons
    , Funcons.Core.Values.Abstraction.Generic.Generic.funcons
    ]
entities = concat 
    [
     Funcons.Core.Computations.Normal.Interacting.Interacting.entities
    , Funcons.Core.Computations.Normal.Flowing.Flowing.entities
    , Funcons.Core.Computations.Normal.Storing.Storing.entities
    , Funcons.Core.Computations.Normal.Linking.Linking.entities
    , Funcons.Core.Computations.Normal.Giving.Giving.entities
    , Funcons.Core.Computations.Normal.Generating.Generating.entities
    , Funcons.Core.Computations.Normal.Binding.Binding.entities
    , Funcons.Core.Computations.Abnormal.Abrupting.Abrupting.entities
    , Funcons.Core.Computations.Abnormal.Returning.Returning.entities
    , Funcons.Core.Computations.Abnormal.Controlling.Controlling.entities
    , Funcons.Core.Computations.Abnormal.Throwing.Throwing.entities
    , Funcons.Core.Computations.Abnormal.Failing.Failing.entities
    , Funcons.Core.Computations.Abnormal.Sticking.entities
    , Funcons.Core.Computations.Abnormal.Breaking.Breaking.entities
    , Funcons.Core.Computations.Abnormal.Continuing.Continuing.entities
    , Funcons.Core.Values.Composite.ASTs.ASTs.entities
    , Funcons.Core.Values.Composite.Records.Records.entities
    , Funcons.Core.Values.Composite.Strings.Strings.entities
    , Funcons.Core.Values.Composite.References.References.entities
    , Funcons.Core.Values.Composite.Graphs.Graphs.entities
    , Funcons.Core.Values.Composite.Vectors.Vectors.entities
    , Funcons.Core.Values.Composite.Variants.Variants.entities
    , Funcons.Core.Values.Composite.Trees.Trees.entities
    , Funcons.Core.Values.Composite.Sequences.Sequences.entities
    , Funcons.Core.Values.Composite.Lists.Lists.entities
    , Funcons.Core.Values.Composite.Classes.Classes.entities
    , Funcons.Core.Values.Composite.Objects.Objects.entities
    , Funcons.Core.Values.Composite.Datatypes.Datatypes.entities
    , Funcons.Core.Values.Composite.Tuples.Tuples.entities
    , Funcons.Core.Values.Composite.Bits.Bits.entities
    , Funcons.Core.Values.Primitive.Floats.Floats.entities
    , Funcons.Core.Values.Primitive.Booleans.Booleans.entities
    , Funcons.Core.Values.Primitive.Null.Null.entities
    , Funcons.Core.Values.Primitive.Characters.Characters.entities
    , Funcons.Core.Values.Primitive.Integers.Integers.entities
    , Funcons.Core.Values.ValueTypes.ValueTypes.entities
    , Funcons.Core.Values.Abstraction.Thunks.Thunks.entities
    , Funcons.Core.Values.Abstraction.Patterns.Patterns.entities
    , Funcons.Core.Values.Abstraction.Functions.Functions.entities
    , Funcons.Core.Values.Abstraction.Generic.Generic.entities
    ]
types = typeEnvUnions 
    [
     Funcons.Core.Computations.Normal.Interacting.Interacting.types
    , Funcons.Core.Computations.Normal.Flowing.Flowing.types
    , Funcons.Core.Computations.Normal.Storing.Storing.types
    , Funcons.Core.Computations.Normal.Linking.Linking.types
    , Funcons.Core.Computations.Normal.Giving.Giving.types
    , Funcons.Core.Computations.Normal.Generating.Generating.types
    , Funcons.Core.Computations.Normal.Binding.Binding.types
    , Funcons.Core.Computations.Abnormal.Abrupting.Abrupting.types
    , Funcons.Core.Computations.Abnormal.Returning.Returning.types
    , Funcons.Core.Computations.Abnormal.Controlling.Controlling.types
    , Funcons.Core.Computations.Abnormal.Throwing.Throwing.types
    , Funcons.Core.Computations.Abnormal.Failing.Failing.types
    , Funcons.Core.Computations.Abnormal.Sticking.types
    , Funcons.Core.Computations.Abnormal.Breaking.Breaking.types
    , Funcons.Core.Computations.Abnormal.Continuing.Continuing.types
    , Funcons.Core.Values.Composite.ASTs.ASTs.types
    , Funcons.Core.Values.Composite.Records.Records.types
    , Funcons.Core.Values.Composite.Strings.Strings.types
    , Funcons.Core.Values.Composite.References.References.types
    , Funcons.Core.Values.Composite.Graphs.Graphs.types
    , Funcons.Core.Values.Composite.Vectors.Vectors.types
    , Funcons.Core.Values.Composite.Variants.Variants.types
    , Funcons.Core.Values.Composite.Trees.Trees.types
    , Funcons.Core.Values.Composite.Sequences.Sequences.types
    , Funcons.Core.Values.Composite.Lists.Lists.types
    , Funcons.Core.Values.Composite.Classes.Classes.types
    , Funcons.Core.Values.Composite.Objects.Objects.types
    , Funcons.Core.Values.Composite.Datatypes.Datatypes.types
    , Funcons.Core.Values.Composite.Tuples.Tuples.types
    , Funcons.Core.Values.Composite.Bits.Bits.types
    , Funcons.Core.Values.Primitive.Floats.Floats.types
    , Funcons.Core.Values.Primitive.Booleans.Booleans.types
    , Funcons.Core.Values.Primitive.Null.Null.types
    , Funcons.Core.Values.Primitive.Characters.Characters.types
    , Funcons.Core.Values.Primitive.Integers.Integers.types
    , Funcons.Core.Values.ValueTypes.ValueTypes.types
    , Funcons.Core.Values.Abstraction.Thunks.Thunks.types
    , Funcons.Core.Values.Abstraction.Patterns.Patterns.types
    , Funcons.Core.Values.Abstraction.Functions.Functions.types
    , Funcons.Core.Values.Abstraction.Generic.Generic.types
    ]
