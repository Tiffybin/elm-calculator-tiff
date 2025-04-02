module Main exposing (..)

import Browser
import Dict
import Html exposing (img)
import Html.Attributes as Attr exposing (alt, src)
import Html.Events as Events
import Parser exposing ((|.), (|=), Parser)
import Set
import String


type alias Model =
    { expressionStr : String
    }


type Expression
    = Num Float
    | Operation Operator Expression Expression
    | LetExp Let Variable Equal Expression Operator Expression


type Let
    = LetBinding


type Let2
    = Letbind
    | Variable
    | In


type Equal
    = Equals


type Variable
    = X


type Operator
    = Plus


type Msg
    = UpdateExpression String


main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }


init =
    { expressionStr = "" }


update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateExpression newExpressionsStr ->
            let
                _ =
                    Debug.log "Parsed expression" (Parser.run parser newExpressionsStr)
            in
            { model | expressionStr = newExpressionsStr }



-- https://bnfplayground.pauliankline.com/?bnf=%3Cnumber%3E%20%3A%3A%3D%20%5B0-9%5D%2B%0A%3Coperation%3E%20%3A%3A%3D%20%22%2B%22%0A%3Cexpression%3E%20%3A%3A%3D%20%3Coperation%3E%20%22%20%22%20%3Cexpression%3E%20%22%20%22%20%3Cexpression%3E%20%7C%20%3Cnumber%3E%0A&name=


intrepret : Expression -> Float
intrepret expr =
    case expr of
        Num n ->
            n

        Operation op ex1 ex2 ->
            case op of
                Plus ->
                    intrepret ex1 + intrepret ex2

        LetExp letWord var eq ex1 op ex2 ->
            case op of
                Plus ->
                    intrepret ex1 + intrepret ex2



-- | LetExp Let Variable Equal Expression Operator Expression Expression


parser : Parser Expression
parser =
    Parser.oneOf
        [ numParser
        , Parser.succeed Operation
            |= operator
            |. Parser.spaces
            |= Parser.lazy (\_ -> parser)
            |. Parser.spaces
            |= Parser.lazy (\_ -> parser)
        , Parser.succeed LetExp
            |= lets
            |. Parser.spaces
            |= variable
            |. Parser.spaces
            |= equals
            |= Parser.lazy (\_ -> parser)
            |. Parser.spaces
            |= operator
            |. Parser.spaces
            |= Parser.lazy (\_ -> parser)
        ]


lets : Parser Let
lets =
    Parser.keyword "let" |> Parser.map (\_ -> LetBinding)


equals : Parser Equal
equals =
    Parser.symbol "=" |> Parser.map (\_ -> Equals)


variable : Parser Variable
variable =
    Parser.symbol "x" |> Parser.map (\_ -> X)


numParser : Parser Expression
numParser =
    Parser.succeed Num
        |= Parser.float


operator : Parser Operator
operator =
    Parser.symbol "+" |> Parser.map (\_ -> Plus)


view : Model -> Html.Html Msg
view model =
    Html.div []
        [ -- [ Html.label []
          --     [ Html.text "Program: "
          --     , Html.textarea [ Attr.value model.expressionStr, Events.onInput UpdateExpression ] []
          --     , Html.text "ans:"
          --     , let
          --         result =
          --             Parser.run parser model.expressionStr
          --       in
          --       case result of
          --         Ok exp ->
          --             Html.text (String.fromFloat (intrepret exp))
          --         Err r ->
          --             Html.span [ Attr.style "color" "red" ] [ Html.text (Debug.toString r) ]
          --     ]
          -- , Html.div []
          --     [ Html.text "Inputs: "
          --     ]
          img [ src "/src/assets/rr.gif" ] []
        , Html.label [] [ Html.text "You got rick rolled" ]
        ]
