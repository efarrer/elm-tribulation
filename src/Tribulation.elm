module Main exposing (..)

import Html exposing (Html, div, a, header, span)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Color exposing (Color)
import Array
import Array.Extra
import List
import List.Split
import Random


width =
    7


height =
    7


type alias Index =
    Int


type alias Cell =
    { value : Int
    , index : Index
    , selected : Int
    , available : Bool
    }


type OpValue
    = Add
    | Subtract
    | Multiply
    | Divide


type OpIndex
    = First
    | Second


type alias Op =
    { value : Maybe OpValue
    , index : OpIndex
    }


type alias Board =
    Array.Array Cell


type alias Model =
    { board : Board
    , first : Op
    , second : Op
    }


type Msg
    = InitBoard (List Int)
    | Selected Index
    | OperatorSelected OpValue OpIndex
    | Success
    | Failure


init : ( Model, Cmd Msg )
init =
    ( { board = Array.fromList [] {- Start with an empty board, it will get populated with InitBoard -}
      , first = { value = Nothing, index = First }
      , second = { value = Nothing, index = Second }
      }
    , Random.generate InitBoard <|
        Random.list (width * height) <|
            (Random.int 1 9)
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InitBoard lst ->
            let
                cells =
                    List.indexedMap
                        (\index value ->
                            { value = value
                            , index = index
                            , selected = 0
                            , available = True
                            }
                        )
                        lst
            in
                ( { model | board = Array.fromList cells }, Cmd.none )

        Selected index ->
            case Array.get index model.board of
                Nothing ->
                    ( model, Cmd.none )

                Just cell ->
                    let
                        value =
                            if cell.selected == 0 then
                                Array.foldl (.selected >> (+)) 1 model.board
                            else
                                0
                    in
                        ( { model | board = updateAvailable <| updateSelected index value model.board }, Cmd.none )

        OperatorSelected opValue opIndex ->
            let
                op =
                    { value = Just opValue, index = opIndex }

                updateModel =
                    case opIndex of
                        First ->
                            { model | first = op }

                        Second ->
                            { model | second = op }
            in
                ( updateModel, Cmd.none )

        Success ->
            ( model, Cmd.none )

        Failure ->
            ( model, Cmd.none )


opToFunc : OpValue -> (Int -> Int -> Int)
opToFunc op =
    case op of
        Add ->
            (+)

        Subtract ->
            (-)

        Multiply ->
            (*)

        Divide ->
            (//)


apply : List Cell -> List OpValue -> Int
apply cells ops =
    case ( cells, ops ) of
        ( a :: rest, op :: ops ) ->
            (opToFunc op) a.value (apply rest ops)

        default ->
            0


finished : Model -> Cmd Msg
finished model =
    case ( model.first.value, model.second.value ) of
        ( Nothing, _ ) ->
            Cmd.none

        ( _, Nothing ) ->
            Cmd.none

        ( Just a, Just b ) ->
            if apply (getOperands model.board) [ a, b ] == 0 then
                Cmd.none
            else
                Cmd.none


indexToCoordinates : Int -> ( Int, Int )
indexToCoordinates index =
    let
        x =
            index % width

        y =
            index // width
    in
        ( x, y )


coordinatesToIndicies : ( Int, Int ) -> Int
coordinatesToIndicies ( x, y ) =
    (y * width) + x


squareCoordinates : List ( Int, Int )
squareCoordinates =
    [ ( 0, 0 ), ( -1, 0 ), ( 1, 0 ), ( 0, -1 ), ( -1, -1 ), ( 1, -1 ), ( 0, 1 ), ( -1, 1 ), ( 1, 1 ) ]


removeInvalidCoords : List ( Int, Int ) -> List ( Int, Int )
removeInvalidCoords =
    List.filter (\( x, y ) -> x >= 0 && x < width && y >= 0 && y < height)


squareIndicies : Int -> Array.Array Int
squareIndicies i =
    let
        ( xi, yi ) =
            indexToCoordinates i
    in
        Array.fromList <|
            List.map coordinatesToIndicies <|
                removeInvalidCoords <|
                    List.map
                        (\( x, y ) -> ( x + xi, y + yi ))
                        squareCoordinates


lineOfThree : ( Int, Int ) -> ( Int, Int ) -> Array.Array ( Int, Int )
lineOfThree ( x0, y0 ) ( x1, y1 ) =
    Array.fromList <|
        removeInvalidCoords
            [ ( x1 + (x1 - x0), y1 + (y1 - y0) )
            , ( x0, y0 )
            , ( x1, y1 )
            ]


updateAvailable : Board -> Board
updateAvailable board =
    let
        selected =
            Array.filter (\c -> c.selected > 0) board

        availableIndicies =
            case Array.toList selected of
                [ _ ] ->
                    selected
                        |> Array.map .index
                        |> Array.foldl (\i rest -> Array.append rest <| squareIndicies i) (Array.fromList [])

                [ a, b ] ->
                    (if a.selected < b.selected then
                        identity
                     else
                        flip
                    )
                        lineOfThree
                        (indexToCoordinates a.index)
                        (indexToCoordinates b.index)
                        |> Array.map coordinatesToIndicies

                [ _, _, _ ] ->
                    Array.map .index selected

                default ->
                    Array.map .index board
    in
        Array.foldl (\i b -> Array.Extra.update i (\cell -> { cell | available = True }) b) (Array.map (\cell -> { cell | available = False }) board) availableIndicies


getOperands : Board -> List Cell
getOperands board =
    List.sortBy .selected <| Array.toList <| Array.filter (\c -> c.selected > 0) board


updateSelected : Int -> Int -> Board -> Board
updateSelected index value board =
    Array.Extra.update index
        (\cell -> { cell | selected = value })
        board


view : Model -> Html Msg
view model =
    div [ class "center" ]
        [ header [ class "title" ] [ Html.text "TRIBULATION" ]
        , viewBoard model.board
        , viewOperatorModal (getOperands model.board) model.first model.second
        ]


viewBoard : Board -> Html Msg
viewBoard board =
    let
        rows =
            List.Split.chunksOfLeft width <| Array.toList board
    in
        viewRows rows


viewRows : List (List Cell) -> Html Msg
viewRows rows =
    div [ class "board" ] <|
        List.map (\row -> viewRow row) rows


viewRow : List Cell -> Html Msg
viewRow row =
    div [ class "row" ] <|
        List.map
            (\cell -> viewCell cell True)
            row


cellClass : Cell -> String
cellClass cell =
    "cell"
        ++ (if cell.selected > 0 then
                " selected"
            else
                ""
           )
        ++ (if cell.available then
                ""
            else
                " unavailable"
           )


viewCell : Cell -> Bool -> Html Msg
viewCell cell interactive =
    div
        [ class (cellClass cell)
        , if interactive then
            onClick <| Selected cell.index
          else
            class ""
        ]
        [ Html.text <| toString cell.value ]


viewOperatorModal : List Cell -> Op -> Op -> Html Msg
viewOperatorModal cells first second =
    case cells of
        [ c0, c1, c2 ] ->
            div [ class "modal" ]
                [ div [ class "modal-content" ]
                    [ div [ class "row" ] <|
                        List.concat
                            [ [ viewCell c0 False ], viewOperator first, [ viewCell c1 False ], viewOperator second, [ viewCell c2 False ] ]
                    ]
                ]

        default ->
            Html.text ""


opToString : OpValue -> String
opToString opValue =
    case opValue of
        Add ->
            "+"

        Subtract ->
            "-"

        Multiply ->
            "*"

        Divide ->
            "/"


viewOperator : Op -> List (Html Msg)
viewOperator op =
    let
        opList =
            case op.value of
                Nothing ->
                    List.map (\opValue -> { value = opValue, index = op.index }) [ Add, Subtract, Multiply, Divide ]

                Just o ->
                    [ { value = o, index = op.index } ]
    in
        List.map (\op -> div [ class "operators", onClick <| OperatorSelected op.value op.index ] [ Html.text <| opToString op.value ]) opList


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
