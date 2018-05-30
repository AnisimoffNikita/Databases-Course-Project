module API.Quiz.Handler where

import           Control.Exception
import           Control.Monad
import           Control.Monad.Reader
import           Control.Monad.IO.Class
import qualified Data.ByteString.Lazy          as LBS
import           Data.ByteString.Lazy           ( toStrict )
import           Data.Maybe                     ( isJust )
import           Data.Text                      ( Text
                                                , unpack
                                                , pack
                                                )
import           Data.Text.Encoding             ( decodeUtf8 )
import           Data.Time.Clock
import qualified Database.MongoDB as Driver 
import           Database.MongoDB(Field((:=)), select, find, Document, rest)
import           Database.Persist.MongoDB
import           Servant
import           Servant.Auth.Server
import           Servant.Multipart

import           API.Quiz.API
import           API.Types
import           API.Utils
import           Model.Model
import           Types
import           Utils
import           Debug.Trace
import           System.Random.Shuffle


quizAPI :: AuthResult JWTData -> ServerT QuizAPI AppM
quizAPI authResult =
  newQuiz authResult
    :<|> getQuizies authResult
    :<|> getAllQuizies authResult
    :<|> getPassed authResult
    :<|> getById authResult
    :<|> search authResult
    :<|> removeById authResult
    :<|> resultId authResult



newQuiz :: AuthResult JWTData -> Quiz -> AppM NoContent
newQuiz (Authenticated user) quiz = do
  let getByUsername = getBy . UniqueUsername . jwtUsername $ user
  pool  <- asks connectionPool
  mUser <- liftIO $ runMongoDBPoolDef getByUsername pool
  case mUser of
    Nothing               -> throwError $ err401 { errBody = "error" }
    Just (Entity id user) -> do
      let actionInsert = insert quiz
      qId <- liftIO $ runMongoDBPoolDef actionInsert pool
      let actionPush = update id [push UserCreatedQuizzes qId]
      liftIO $ runMongoDBPoolDef actionPush pool
      return NoContent
newQuiz _ _ = throwError $ err400 { errBody = "error" }


getQuizies :: AuthResult JWTData -> AppM [QuizPreview]
getQuizies (Authenticated user) = do
  let getByUsername = getBy . UniqueUsername . jwtUsername $ user
  pool  <- asks connectionPool
  mUser <- liftIO $ runMongoDBPoolDef getByUsername pool
  case mUser of
    Nothing               -> throwError $ err401 { errBody = "error" }
    Just (Entity id user) -> do
      let selectAll = selectList [QuizId <-. (userCreatedQuizzes user)] []
      pool <- trace (show (userCreatedQuizzes user)) $ asks connectionPool
      quizies :: [Entity Quiz] <- liftIO $ runMongoDBPoolDef selectAll pool
      return $ map
        (\(Entity (QuizKey (MongoKey id)) quiz) ->
          quizToPreview (pack . show $ id) quiz
        )
        quizies
getQuizies x = throwError $ err400 { errBody = "error" }


getAllQuizies :: AuthResult JWTData -> AppM [QuizPreview]
getAllQuizies (Authenticated user) = do
  let getByUsername = getBy . UniqueUsername . jwtUsername $ user
  pool  <- asks connectionPool
  mUser <- liftIO $ runMongoDBPoolDef getByUsername pool
  case mUser of
    Nothing               -> throwError $ err401 { errBody = "error" }
    Just (Entity id user) -> do
      let selectAll = selectList [] []
      pool                     <- asks connectionPool
      quizies :: [Entity Quiz] <- liftIO $ runMongoDBPoolDef selectAll pool
      return $ map
        (\(Entity (QuizKey (MongoKey id)) quiz) ->
          quizToPreview (pack . show $ id) quiz
        )
        quizies
getAllQuizies _ = throwError $ err400 { errBody = "error" }


getPassed :: AuthResult JWTData -> AppM [QuizPreviewResult]
getPassed (Authenticated user) = do
  let getByUsername = getBy . UniqueUsername . jwtUsername $ user
  pool  <- asks connectionPool
  mUser <- liftIO $ runMongoDBPoolDef getByUsername pool
  case mUser of
    Nothing               -> throwError $ err401 { errBody = "error" }
    Just (Entity id user) -> do
      let ids = map quizResultQuizid (userPassedQuizzes user)
      let results = map quizResultResult (userPassedQuizzes user)
      let selectAll = selectList [QuizId <-. ids] []
      pool <- asks connectionPool
      quizies :: [Entity Quiz] <- liftIO $ runMongoDBPoolDef selectAll pool
      return $ map
        (\((Entity (QuizKey (MongoKey id)) quiz), result) ->
          quizToPreviewResult result (pack . show $ id) quiz
        )
        (zip quizies results)
getPassed _ = throwError $ err400 { errBody = "error" }

getById :: AuthResult JWTData -> Text -> AppM QuizWithoutAnswers
getById (Authenticated user) id = do
  let getByUsername = getBy . UniqueUsername . jwtUsername $ user
  pool  <- asks connectionPool
  mUser <- liftIO $ runMongoDBPoolDef getByUsername pool
  case (mUser, readMayMongoKey id) of
    (Just (Entity uId user), Just id) -> do
      let qId    = (QuizKey id)
          action = selectFirst [QuizId ==. qId] []
      pool  <- asks connectionPool
      mQuiz <- liftIO $ runMongoDBPoolDef action pool
      case mQuiz of
        Just (Entity _ quiz) -> do --
          let uquiz = quizWithoutAnswers quiz
          let questions = dataQuestions uquiz
          let 
            f q@QuestionWithoutAnswer{..} = do
              newVariants <- shuffleM variants
              return $ q {variants = newVariants}
          newQuestions <- liftIO $ mapM f questions
          return $ uquiz {dataQuestions = newQuestions}

        Nothing              -> throwError $ err404 { errBody = "error" }
    _ -> throwError $ err401 { errBody = "error" }
getById _ _ = throwError $ err400 { errBody = "error" }

search :: AuthResult JWTData -> Text -> AppM [QuizPreview]
search (Authenticated user) query = do
  let getByUsername = getBy . UniqueUsername . jwtUsername $ user
  pool  <- asks connectionPool
  mUser <- liftIO $ runMongoDBPoolDef getByUsername pool
  case mUser of
    Nothing               -> throwError $ err401 { errBody = "error" }
    Just (Entity id user) -> do
      let selectAll = (rest =<< find (select ["$text" =: [ "$search" =: query]] "quiz") :: Action IO [Document])
      pool                     <- asks connectionPool
      eQuizies :: Either  Text [Entity Quiz] <- liftIO $ mapM docToEntityEither <$> runMongoDBPoolDef selectAll pool
      case eQuizies of 
        Right quizies -> 
          return $ map
            (\(Entity (QuizKey (MongoKey id)) quiz) ->
              quizToPreview (pack . show $ id) quiz
            )
            quizies
        Left _ -> throwError $ err400 { errBody = "error" }
search _ _ = throwError $ err400 { errBody = "error" }

searchHelper :: Text -> Driver.Document -> Action IO Driver.Document
searchHelper term filter =
  Driver.runCommand ["text" =: ("quiz" :: Text), "search" =: term, "filter" =: filter]

removeById :: AuthResult JWTData -> Text -> AppM NoContent
removeById (Authenticated user) id = do
  let getByUsername = getBy . UniqueUsername . jwtUsername $ user
  pool  <- asks connectionPool
  mUser <- liftIO $ runMongoDBPoolDef getByUsername pool
  case (mUser, readMayMongoKey id) of
    (Just (Entity uId user), Just id) -> do
      let qId          = (QuizKey id)
          actionRemove = delete qId
          actionPull   = update uId [pull UserCreatedQuizzes qId]
      pool <- asks connectionPool
      liftIO $ runMongoDBPoolDef actionRemove pool
      liftIO $ runMongoDBPoolDef actionPull pool
      return NoContent
    _ -> throwError $ err401 { errBody = "error" }
removeById _ _ = throwError $ err400 { errBody = "error" }


resultId :: AuthResult JWTData -> Text -> [Text] -> AppM Text
resultId (Authenticated user) id answers = do
  let getByUsername = getBy . UniqueUsername . jwtUsername $ user
  pool  <- asks connectionPool
  mUser <- liftIO $ runMongoDBPoolDef getByUsername pool
  case (mUser, readMayMongoKey id) of
    (Just (Entity uId user), Just key) -> do
      let qId    = (QuizKey key)
          action = selectFirst [QuizId ==. qId] []
      pool  <- asks connectionPool
      mQuiz <- liftIO $ runMongoDBPoolDef action pool
      case mQuiz of
        Just (Entity _ quiz) -> do
          time <- liftIO getCurrentTime
          let
            correct = map questionAnswer (quizQuestions quiz)
            result  = length $ filter (\(a, b) -> a == b) (zip answers correct)
            resultString =
              (show result) ++ "/" ++ (show . length $ quizQuestions quiz)
            quizResult = QuizResult qId (pack resultString) time
            actionPush = update uId [push UserPassedQuizzes quizResult]
          liftIO $ runMongoDBPoolDef actionPush pool
          return $ pack resultString
        Nothing -> throwError $ err404 { errBody = "error" }
    _ -> throwError $ err401 { errBody = "error" }
resultId _ _ _ = throwError $ err400 { errBody = "error" }


