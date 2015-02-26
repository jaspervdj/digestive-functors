--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module Text.Digestive.Tests.Fixtures
    ( -- * Pokemon!
      TrainerM
    , runTrainerM
    , Type (..)
    , Pokemon (..)
    , pokemonForm
    , ValidateOptionalData (..)
    , validateOptionalForm
    , IndependentValidationsData (..)
    , independentValidationsForm
    , Ball (..)
    , ballForm
    , Catch (..)
    , catchForm

      -- * Store/product
    , Database
    , runDatabase
    , sector9
    , earthwing
    , comet
    , Product (..)
    , productForm
    , Order (..)
    , orderForm
    , ordersForm

      -- * Various
    , floatForm
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative          ((<$>), (<*>))
import           Control.Monad                ((>=>))
import           Control.Monad.Reader         (Reader, ask, runReader)
import           Data.Text                    (Text)
import qualified Data.Text                    as T


--------------------------------------------------------------------------------
import           Text.Digestive.Form
import           Text.Digestive.Types
import           Text.Digestive.Util


--------------------------------------------------------------------------------
-- Maximum level
type TrainerM = Reader Int


--------------------------------------------------------------------------------
-- Default max level: 20
runTrainerM :: TrainerM a -> a
runTrainerM = flip runReader 20


--------------------------------------------------------------------------------
data Type = Water | Fire | Leaf
    deriving (Eq, Show)


--------------------------------------------------------------------------------
typeForm :: Monad m => Form Text m Type
typeForm = choice [(Water, "Water"), (Fire, "Fire"), (Leaf, "Leaf")] Nothing

typesForm :: Monad m => Form Text m [Type]
typesForm = choices [(Water, "Water"), (Fire, "Fire"), (Leaf, "Leaf")] Nothing

--------------------------------------------------------------------------------
data Pokemon = Pokemon
    { pokemonName  :: Text
    , pokemonLevel :: Maybe Int
    , pokemonType  :: Type
    , pokemonTypes  :: [Type]
    , pokemonRare  :: Bool
    } deriving (Eq, Show)


--------------------------------------------------------------------------------
levelForm :: Form Text TrainerM (Maybe Int)
levelForm =
    checkM "This pokemon will not obey you!" checkMaxLevel      $
    check  "Level should be at least 1"      (maybe True (> 1)) $
    optionalStringRead "Cannot parse level" Nothing
  where
    checkMaxLevel Nothing  = return True
    checkMaxLevel (Just l) = do
        maxLevel <- ask
        return $ l <= maxLevel


--------------------------------------------------------------------------------
pokemonForm :: Form Text TrainerM Pokemon
pokemonForm = Pokemon
    <$> "name"  .: validate isPokemon (text Nothing)
    <*> "level" .: levelForm
    <*> "type"  .: typeForm
    <*> "types"  .: typesForm
    <*> "rare"  .: bool Nothing
  where
    definitelyNoPokemon = ["dog", "cat"]
    isPokemon name
        | name `notElem` definitelyNoPokemon = Success name
        | otherwise                          =
            Error $ name `T.append` " is not a pokemon!"


--------------------------------------------------------------------------------
data Ball = Poke | Great | Ultra | Master
    deriving (Eq, Show)


--------------------------------------------------------------------------------
ballForm :: Monad m => Form Text m Ball
ballForm = choice
    [(Poke, "Poke"), (Great, "Great"), (Ultra, "Ultra"), (Master, "Master")]
    Nothing


--------------------------------------------------------------------------------
data ValidateOptionalData = ValidateOptionalData
    { firstValidateOptionalField  :: Maybe Integer
    } deriving (Eq, Show)


--------------------------------------------------------------------------------
validateOptionalForm :: Monad m => Form Text m ValidateOptionalData
validateOptionalForm = ValidateOptionalData
    <$> "first_field"  .: validateOptional (integer >=> even' >=> greaterThan 0) (optionalString Nothing)
  where
    integer s = maybe (Error "not an integer") Success (readMaybe s)
    even' n
      | n `mod` 2 == 0 = Success n
      | otherwise      = Error "input must be even"
    greaterThan x n
      | n > x     = Success n
      | otherwise = Error "input is too small"


--------------------------------------------------------------------------------
data IndependentValidationsData = IndependentValidationsData
    { firstIndependentValidationField :: Integer
    } deriving (Eq, Show)


--------------------------------------------------------------------------------
independentValidationsForm :: Monad m => Form [Text] m IndependentValidationsData
independentValidationsForm = IndependentValidationsData
    <$> "first_field"  .: validate (notEmpty >=> integer >=> conditions [even', greaterThan 10]) (string Nothing)
  where
    notEmpty x = if (null x)
      then Error ["is empty"]
      else Success x
    integer s = maybe (Error ["not an integer"]) Success (readMaybe s)
    even' n
      | n `mod` 2 == 0 = Success n
      | otherwise      = Error "input must be even"
    greaterThan x n
      | n > x     = Success n
      | otherwise = Error "input is too small"


--------------------------------------------------------------------------------
data Catch = Catch
    { catchPokemon :: Pokemon
    , catchBall    :: Ball
    } deriving (Eq, Show)


--------------------------------------------------------------------------------
catchForm :: Form Text TrainerM Catch
catchForm = check "You need a better ball" canCatch $ Catch
    <$> "pokemon" .: pokemonForm
    <*> "ball"    .: ballForm


--------------------------------------------------------------------------------
canCatch :: Catch -> Bool
canCatch (Catch (Pokemon _ _ _ _ False) _)      = True
canCatch (Catch (Pokemon _ _ _ _ True)  Ultra)  = True
canCatch (Catch (Pokemon _ _ _ _ True)  Master) = True
canCatch _                                    = False


--------------------------------------------------------------------------------
type Database = Reader [Product]


--------------------------------------------------------------------------------
runDatabase :: Database a -> a
runDatabase = flip runReader [sector9, earthwing, comet]


--------------------------------------------------------------------------------
sector9 :: Product
sector9 = Product "s9_ao" "Sector 9 Agent Orange"


--------------------------------------------------------------------------------
earthwing :: Product
earthwing = Product "ew_br" "Earthwing Belly Racer"


--------------------------------------------------------------------------------
comet :: Product
comet = Product "cm_gs" "Comet Grease Shark"


--------------------------------------------------------------------------------
data Product = Product
    { productId   :: Text
    , productName :: Text
    } deriving (Eq, Show)


--------------------------------------------------------------------------------
productForm :: Formlet Text Database Product
productForm def = monadic $ do
    products <- ask
    return $ choiceWith (map makeChoice products) def
  where
    makeChoice p = (productId p, (p, productName p))


--------------------------------------------------------------------------------
data Order = Order
    { orderProduct  :: Product
    , orderQuantity :: Int
    } deriving (Eq, Show)


--------------------------------------------------------------------------------
orderForm :: Formlet Text Database Order
orderForm def = Order
    <$> "product"  .: productForm              (orderProduct <$> def)
    <*> "quantity" .: stringRead "Can't parse" (orderQuantity <$> def)


--------------------------------------------------------------------------------
ordersForm :: Formlet Text Database (Text, [Order])
ordersForm def = (,)
    -- This field is disabled.
    <$> disable ("name"   .: text            (fst <$> def))
    -- id is here because of a regression
    <*> (id <$> "orders" .: listOf orderForm (snd <$> def))


--------------------------------------------------------------------------------
floatForm :: Monad m => Form Text m Float
floatForm = "f" .: stringRead "Can't parse float" Nothing
