-- | Yesod.Test.Json provides convenience functions for working
--   with Test.Hspec and Network.Wai.Test on JSON data.
module Yesod.Test.Json (
	testApp,
	APIFunction,
	assertBool,
	assertString,
	assertOK,
	assertJSON,
	Session(..),
	H.Assertion,
	module Test.Hspec,
	module Data.Aeson,
        SResponse(..)
	) where
import qualified Test.HUnit as H
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Lazy as L
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Aeson
import Network.HTTP.Types
import Test.Hspec
import Network.Wai
import Network.Wai.Test
import Control.Monad.IO.Class
import Yesod.Default.Config
import Data.Conduit.List

-- | A request to your server. 
type APIFunction = ByteString -- ^ method
                   -> [Text] -- ^ path
                   -> Maybe Value -- JSON data
                   -> Session SResponse

-- Assert a boolean value
assertBool :: String -> Bool -> Session ()
assertBool s b = liftIO $ H.assertBool s b

-- Fail a test with an error string
assertString :: String -> Session ()
assertString = liftIO . H.assertString

-- Assert a 200 response code
assertOK :: SResponse -> Session ()
assertOK SResponse{simpleStatus = s, simpleBody = b} = assertBool (concat
    [ "Expected status code 200, but received " 
    , show sc
    , ". Response body: "
    , show (L8.unpack b)
    ]) $ sc == 200
  where
    sc = statusCode s

-- Assert a JSON body meeting some requirement
assertJSON :: (ToJSON a, FromJSON a) => (a -> (String, Bool)) -> SResponse -> Session ()
assertJSON f SResponse{simpleBody = lbs} = do
	case decode lbs of
		Nothing -> assertString $ "Invalid JSON: "  ++ show (L8.unpack lbs)
		Just a ->
			case fromJSON a of
				Error s -> assertString (concat [s, "\nInput JSON: ", show a])
				Success x -> uncurry assertBool (f x)

-- | Make a request to your server
apiRequest :: AppConfig env extra -> APIFunction
apiRequest conf m p x = srequest $ SRequest r (maybe L.empty encode x) where
    r = defaultRequest {
        serverPort = appPort conf,
        requestBody = sourceList . L.toChunks $ encode x,
        requestMethod = m,
        pathInfo = p
    }

-- | Run a test suite for your 'Application'
testApp :: Application -> AppConfig env extra ->
		  (((APIFunction -> Session ()) -> H.Assertion) -> Spec) -> IO ()
testApp app conf specfun = do
	let apiTest f = runSession (f (apiRequest conf)) app
	hspec $ (specfun apiTest)
