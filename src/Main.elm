port module Main exposing (..)

import Platform
import Dict exposing (Dict)
import Time exposing (Time, second)
import Process
import Task exposing (Task)
import ParentChildUpdate exposing (..)
import Slate.Common.Db exposing (..)
import Slate.Common.Event exposing (..)
import Slate.Command.Common.Command exposing (..)
import Slate.Command.Common.Validation exposing (..)
import Slate.Command.Helper exposing (..)
import Slate.Command.Processor as CommandProcessor exposing (..)
import Slate.Command.Common.Validation exposing (..)
import Utils.Ops exposing (..)
import Utils.Error exposing (..)
import Utils.Log exposing (..)
import StringUtils exposing ((+-+), (+++))
import DebugF exposing (..)


port exitApp : Float -> Cmd msg


port externalStop : (() -> msg) -> Sub msg


dbConnectionInfo : DbConnectionInfo
dbConnectionInfo =
    { host = "testEntitiesServer"
    , port_ = 5432
    , database = "test_entities"
    , user = "postgres"
    , password = "password"
    , timeout = 15000
    }


commandProcessorConfig : CommandProcessor.Config String Msg
commandProcessorConfig =
    { routeToMeTagger = CommandProcessorModule
    , errorTagger = CommandProcessorError
    , logTagger = CommandProcessorLog
    , commandErrorTagger = CommandError
    , commandSuccessTagger = CommandSuccess
    }


type alias Model =
    { commandProcessorModel : CommandProcessor.Model String Msg
    , commands : List ( EntityLockAndValidation, String, Maybe (ValidateTagger (CommandProcessor.Msg String) String Msg) )
    }


type Msg
    = Nop
    | DoCmd (Cmd Msg)
    | StartApp
    | NextCommand
    | CommandProcessorError ( ErrorType, ( CommandId, String ) )
    | CommandProcessorLog ( LogLevel, ( CommandId, String ) )
    | CommandProcessorModule (CommandProcessor.Msg String)
    | CommandError ( CommandId, CommandError String )
    | CommandSuccess CommandId
    | CommandsComplete
    | ValidateAddName String (CustomValidationErrorTagger String (CommandProcessor.Msg String)) (CustomValidationSuccessTagger (CommandProcessor.Msg String)) CommandId DbConnectionInfo
    | ValidateNames (List String) (CustomValidationErrorTagger String (CommandProcessor.Msg String)) (CustomValidationSuccessTagger (CommandProcessor.Msg String)) CommandId DbConnectionInfo


initModel : ( Model, List (Cmd Msg) )
initModel =
    let
        ( commandProcessorModel, commandProcessorCmd ) =
            CommandProcessor.init commandProcessorConfig
    in
        ( { commandProcessorModel = commandProcessorModel
          , commands = []
          }
        , [ commandProcessorCmd ]
        )


init : ( Model, Cmd Msg )
init =
    let
        ( model, cmds ) =
            initModel
    in
        model ! (List.append cmds [ delayUpdateMsg StartApp (1 * second) ])


delayUpdateMsg : Msg -> Time -> Cmd Msg
delayUpdateMsg msg delay =
    Task.perform (\_ -> msg) <| Process.sleep delay


delayCmd : Cmd Msg -> Time -> Cmd Msg
delayCmd cmd =
    delayUpdateMsg <| DoCmd cmd


main : Program Never Model Msg
main =
    Platform.program
        { init = init
        , update = update
        , subscriptions = subscriptions
        }


fromDict : Dict String a -> String -> a
fromDict dict key =
    Dict.get key dict ?!= (\_ -> Debug.crash ("Unable to find key:" +-+ key +-+ "in dict:" +-+ dict))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        updateCommandProcessor =
            updateChildApp (CommandProcessor.update commandProcessorConfig) update .commandProcessorModel CommandProcessorModule (\model commandProcessorModel -> { model | commandProcessorModel = commandProcessorModel })
    in
        case msg of
            Nop ->
                model ! []

            DoCmd cmd ->
                model ! [ cmd ]

            StartApp ->
                let
                    {- a single Cmd is a transaction; so each Command is executed in its own transaction. -}
                    asMultCmds =
                        let
                            toEvent mutatingEvent =
                                Mutating mutatingEvent (Metadata "999888777" "asMultCmds")

                            commands =
                                List.map (\( mutatingEvent, maybeVaidation ) -> ( LockAndValidate << ValidateForMutation <| toEvent mutatingEvent, encodeMutatingEvent <| toEvent mutatingEvent, maybeVaidation ))
                                    [ ( CreateEntity "Person" "123", Nothing )
                                    , ( CreateEntity "Person" "456", Nothing )
                                    , ( CreateEntity "Address" "789", Nothing )
                                    , ( CreateEntity "Address" "1111", Nothing )
                                    , ( CreateEntity "Address" "2222", Nothing )
                                    , ( AddProperty "Person" "123" "name" """{"first": "Joe", "middle": "", "last": "Mama"}""", Just <| ValidateAddName "Joe Mama" )
                                    , ( AddProperty "Person" "456" "name" """{"first": "Mickey", "middle": "", "last": "Mouse"}""", Just <| ValidateAddName "Mickey Mouse" )
                                    , ( AddProperty "Address" "789" "street" "Main Street", Nothing )
                                    , ( AddRelationship "Person" "456" "address" "789", Nothing )
                                    , ( RemoveRelationship "Person" "456" "address", Nothing )
                                    , ( AddPropertyList "Person" "123" "aliases" "9001" "Joe Blow", Nothing )
                                    , ( AddPropertyList "Person" "123" "aliases" "9002" "Joe Cool", Nothing )
                                    , ( AddPropertyList "Person" "123" "aliases" "9003" "Joey Joe", Nothing )
                                    , ( PositionPropertyList "Person" "123" "aliases" ( 1, 0 ), Nothing )
                                    , ( AddRelationshipList "Person" "123" "oldAddresses" "8001" "1111", Nothing )
                                    , ( AddRelationshipList "Person" "123" "oldAddresses" "8002" "2222", Nothing )
                                    , ( PositionRelationshipList "Person" "123" "oldAddresses" ( 1, 0 ), Nothing )
                                    , ( DestroyEntity "Person" "123", Nothing )
                                    ]
                        in
                            update NextCommand { model | commands = commands }

                    {- a single Cmd is a transaction; so each Command is executed in a single transaction. -}
                    asOneCmd =
                        let
                            mutatingEvents =
                                [ CreateEntity "Person" "123"
                                , CreateEntity "Person" "456"
                                , CreateEntity "Address" "789"
                                , CreateEntity "Address" "1111"
                                , CreateEntity "Address" "2222"
                                , AddProperty "Person" "123" "name" """{"first": "Joe", "middle": "", "last": "Mama"}"""
                                , AddProperty "Person" "456" "name" """{"first": "Mickey", "middle": "", "last": "Mouse"}"""
                                , AddProperty "Address" "789" "street" "Main Street"
                                , AddRelationship "Person" "456" "address" "789"
                                , RemoveRelationship "Person" "456" "address"
                                , AddPropertyList "Person" "123" "aliases" "9001" "Joe Blow"
                                , AddPropertyList "Person" "123" "aliases" "9002" "Joe Cool"
                                , AddPropertyList "Person" "123" "aliases" "9003" "Joey Joe"
                                , PositionPropertyList "Person" "123" "aliases" ( 1, 0 )
                                , AddRelationshipList "Person" "123" "oldAddresses" "8001" "1111"
                                , AddRelationshipList "Person" "123" "oldAddresses" "8002" "2222"
                                , PositionRelationshipList "Person" "123" "oldAddresses" ( 1, 0 )
                                , DestroyEntity "Person" "123"
                                ]

                            toEvent mutatingEvent =
                                Mutating mutatingEvent (Metadata "999888777" "asOneCmd")

                            ( entityValidations, encodedEvents ) =
                                ( mutatingEvents |> List.map (\mutatingEvent -> (LockAndValidate << ValidateForMutation <| toEvent mutatingEvent))
                                , mutatingEvents |> List.map (encodeMutatingEvent << toEvent)
                                )

                            ( commandProcessorModel, cmd, commandId ) =
                                CommandProcessor.process commandProcessorConfig dbConnectionInfo (Just <| ValidateNames [ "Joe Mama", "Mickey Mouse" ]) ((LockOnly "dummylock") :: entityValidations) encodedEvents model.commandProcessorModel
                        in
                            { model | commandProcessorModel = commandProcessorModel } ! [ cmd ]
                in
                    -- asMultCmds
                    asOneCmd

            NextCommand ->
                case model.commands of
                    ( entityLockAndValidation, encodedEvent, maybeValidation ) :: rest ->
                        let
                            ( commandProcessorModel, cmd, commandId ) =
                                CommandProcessor.process commandProcessorConfig dbConnectionInfo maybeValidation [ entityLockAndValidation ] [ encodedEvent ] model.commandProcessorModel
                        in
                            { model | commands = rest, commandProcessorModel = commandProcessorModel } ! [ cmd ]

                    [] ->
                        update CommandsComplete model

            CommandsComplete ->
                let
                    l =
                        Debug.log "CommandsComplete" ""
                in
                    model ! []

            CommandProcessorError ( errorType, details ) ->
                let
                    l =
                        case errorType of
                            NonFatalError ->
                                DebugF.log "CommandProcessorError" details

                            _ ->
                                Debug.crash <| toString details
                in
                    model ! []

            CommandProcessorLog ( logLevel, details ) ->
                let
                    l =
                        DebugF.log "CommandProcessorLog" (toString logLevel ++ ":" +-+ details)
                in
                    model ! []

            CommandError ( commandId, error ) ->
                let
                    l =
                        DebugF.log "CommandError" ( commandId, error )
                in
                    model ! []

            CommandSuccess commandId ->
                let
                    l =
                        Debug.log "CommandSuccess" commandId
                in
                    update NextCommand model

            CommandProcessorModule msg ->
                updateCommandProcessor msg model

            ValidateAddName name errorTagger successTagger commandId dbConnectionInfo ->
                let
                    l =
                        Debug.log "ValidateAddName" name
                in
                    -- updateCommandProcessor (errorTagger (commandId, "bad things would happen")) model
                    updateCommandProcessor (successTagger commandId) model

            ValidateNames names errorTagger successTagger commandId dbConnectionInfo ->
                let
                    l =
                        Debug.log "ValidateNames" names
                in
                    -- updateCommandProcessor (errorTagger (commandId, "bad things would happen")) model
                    updateCommandProcessor (successTagger commandId) model


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
