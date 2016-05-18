module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Html.App as Html
import Array exposing (fromList, toList)
import Random.Array exposing (shuffle)
import Random as Random
import String as String


type Suit
    = Diamonds
    | Clubs
    | Spades
    | Hearts
    | None


type Rank
    = Two
    | Three
    | Four
    | Five
    | Six
    | Seven
    | Eight
    | Nine
    | Ten
    | Jack
    | Queen
    | King
    | Ace
    | Joker


type alias Card =
    { suit : Suit
    , rank : Rank
    }


type alias Deck =
    List Card


type alias Model =
    { deck : Deck
    }


type Msg
    = NoOp
    | ShuffledDeck Deck
    | ShuffleDeck


rankScoreToRank : Int -> Rank
rankScoreToRank score =
    case score of
        2 ->
            Two

        3 ->
            Three

        4 ->
            Four

        5 ->
            Five

        6 ->
            Six

        7 ->
            Seven

        8 ->
            Eight

        9 ->
            Nine

        10 ->
            Ten

        11 ->
            Jack

        12 ->
            Queen

        13 ->
            King

        14 ->
            Ace

        _ ->
            Joker


suitStringToSuit : String -> Suit
suitStringToSuit suit =
    case suit of
        "D" ->
            Diamonds

        "H" ->
            Hearts

        "S" ->
            Spades

        "C" ->
            Clubs

        _ ->
            None


toCard : Suit -> List Card
toCard suit =
    let
        ranks =
            List.map rankScoreToRank [ 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14 ]
    in
        List.map (\r -> Card suit r) ranks


newDeck : Deck
newDeck =
    let
        suits =
            List.map suitStringToSuit [ "D", "H", "S", "C" ]
    in
        List.concatMap toCard suits


init : ( Model, Cmd Msg )
init =
    ( Model [], Random.generate ShuffledDeck shuffleDeck )


shuffleDeck : Random.Generator Deck
shuffleDeck =
    let
        deck =
            newDeck

        deckArr =
            fromList deck

        shuffler =
            shuffle deckArr
    in
        Random.map (\arr -> toList arr) shuffler


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        ShuffledDeck deck ->
            ( { model | deck = deck }, Cmd.none )

        ShuffleDeck ->
            ( model, Random.generate ShuffledDeck shuffleDeck )


cardToHtml : Card -> Html Msg
cardToHtml card =
    div []
        [ div [ class "suit" ] [ text (toString card.suit) ]
        , div [ class "rank" ] [ text (toString card.rank) ]
        ]


displayDeck : Model -> Html Msg
displayDeck model =
    let
        childNodes =
            List.map cardToHtml model.deck
    in
        div [ class "deck" ] childNodes


newDealButton : Model -> Html Msg
newDealButton model =
    button [ onClick ShuffleDeck ] [ text "Shuffle" ]


cardToString : Card -> String
cardToString card =
    let
        rankText =
            toString card.rank

        joiner =
            " of "

        suitText =
            toString card.suit
    in
        rankText ++ joiner ++ suitText


cardToDiv : Card -> Html Msg
cardToDiv card =
    div [ class "card" ] [ text (cardToString card) ]


cards : Deck -> Html Msg
cards deck =
    let
        cards =
            List.map cardToDiv deck
    in
        div [] cards


view : Model -> Html Msg
view model =
    div []
        [ (cards model.deck)
        , div [ class "startshuffling" ]
            [ button [ onClick ShuffleDeck ] [ text "Shuffle!" ] ]
        ]


main =
    Html.program
        { init = init
        , subscriptions = \_ -> Sub.none
        , update = update
        , view = view
        }
