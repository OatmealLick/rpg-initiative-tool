module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode
import Personas exposing (Persona)
import List.Extra as ListX exposing (last, init)
import Material.Card as Card
import Material.Button as Button
import Material.Options as Options exposing (cs, css)
import Material.Elevation as Elevation
import Material.Color as Color exposing (Hue(..))
import Material.Scheme as Scheme
import Material.List as Lists
import Material.Table as Table
import Material.Icon as Icon
import Material.Dialog as Dialog
import Material
import Material.Textfield as Textfield
import Material.Toggles as Toggles

main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = always Sub.none
    }

-- MODEL
type alias Model =
    {
      activeQueue : List Persona,
      toBeAdded : Persona,
      mdl : Material.Model,
      raised : Int,
      focused : Bool
    }

init : (Model, Cmd Msg)
init =
  (Model (Personas.sortPersonas startupPersonas) (Persona "" Nothing Nothing False) Material.model -1 False, Cmd.none)

startupPersonas : List Persona
startupPersonas =
  [
    Persona "Felix Panafix" (Just 20) (Just 1) True,
    Persona "Dien" (Just 1) (Just 1) True,
    Persona "Valanthe" (Just 17) (Just 1) True,
    Persona "Quallan" (Just -2) (Just 1) True
  ]

-- UPDATE
type Msg
  = Next
  | Remove
  | Previous

  | NewName String
  | NewValue String
  | NewModifier String
  | ToggleFriendly
  | AddPersona
  | Reset

  | Mdl (Material.Msg Msg)
  | Raise Int
  | SetFocusDialogName Bool

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
      -- Queue management
      Next ->
        case model.activeQueue of
          [] -> ({model | activeQueue = model.activeQueue}, Cmd.none)
          h::t -> ({model | activeQueue = t ++ [h]}, Cmd.none)
      Previous ->
        let
          last = ListX.last model.activeQueue
          withoutLast = ListX.init model.activeQueue
        in
          case last of
            Nothing -> ({model | activeQueue = model.activeQueue}, Cmd.none)
            Just persona ->
              case withoutLast of
                Nothing -> ({model | activeQueue = model.activeQueue}, Cmd.none)
                Just frontPersonas ->
                  ({model | activeQueue = persona :: frontPersonas}, Cmd.none)
      Remove ->
        ({model | activeQueue = List.drop 1 model.activeQueue}, Cmd.none)

      -- To be added persona management
      NewName newName ->
        let
          persona = model.toBeAdded
        in
        ({model | toBeAdded = {persona | name = newName}}, Cmd.none)
      NewValue newValue ->
        let
          persona = model.toBeAdded
        in
        ({model | toBeAdded = {persona | value = convertStringToMaybeInt newValue}}, Cmd.none)
      NewModifier newModifier ->
        let
          persona = model.toBeAdded
        in
        ({model | toBeAdded = {persona | modifier = convertStringToMaybeInt newModifier}}, Cmd.none)
      ToggleFriendly ->
        let
          persona = model.toBeAdded
        in
        ({model | toBeAdded = {persona | friendly = not persona.friendly}}, Cmd.none)
      AddPersona ->
        if String.isEmpty model.toBeAdded.name
        then (model, Cmd.none)
        else ({model |
          activeQueue = (Personas.sortPersonas (model.toBeAdded :: model.activeQueue)),
          toBeAdded = (Persona "" Nothing Nothing False)}, Cmd.none)
      Reset ->
        ({model | activeQueue = [], toBeAdded = (Persona "" Nothing Nothing False)}, Cmd.none)

      -- Material calls
      Mdl msg_ ->
            Material.update Mdl msg_ model
      Raise k ->
            { model | raised = k } ! []
      SetFocusDialogName focused ->
            ({ model | focused = focused}, Cmd.none)

convertStringToMaybeInt : String -> Maybe Int
convertStringToMaybeInt text =
  case String.toInt text of
    Ok value -> Just value
    Err _ -> Nothing

convertMaybeIntToString : Maybe Int -> String
convertMaybeIntToString value =
  case value of
    Nothing -> ""
    Just number -> toString number

-- VIEW
view : Model -> Html Msg
view model =
  div []
  [
    viewCurrentPersona model,
    viewQueue (List.drop 1 model.activeQueue),
    viewAddPersona model,
    viewResetQueue model,
    dialog model
  ]

dynamic : Int -> Model -> Options.Style Msg
dynamic k model =
    [ if model.raised == k then
        Elevation.e8
      else
        Elevation.e2
    , Elevation.transition 250
    , Options.onMouseEnter (Raise k)
    , Options.onMouseLeave (Raise -1)
    ]
        |> Options.many


white : Options.Property c m
white =
    Color.text Color.white

viewCurrentPersona : Model -> Html Msg
viewCurrentPersona model =
  case (List.head model.activeQueue) of
    Nothing ->
      div
      [
        style([
          ("margin-left", "auto"),
          ("margin-right", "auto"),
          ("margin-top", "60px"),
          ("width", "400px")])
      ]
      [
        h2 [] [text "No characters in queue!"],
        img
        [
          src "assets/sad_panda.png",
          style([
            ("width", "400px"),
            ("height", "400px"),
            ("margin-top", "60px")
            ])
        ] [],
        p [
          style([("margin-top", "60px")])] [text "Add characters using button with the PLUS SIGN."]
      ]

    Just persona ->
        Card.view
            [ dynamic 1 model,
              css "width" "400px",
              css "margin-left" "auto",
              css "margin-right" "auto",
              css "margin-top" "60px"
            ]
            [ Card.title
                [ css "background" ("url('" ++ getImagePathForName persona.name ++ "') center / cover")
                , css "height" "400px"
                , css "padding" "0"
                ]
                [ Card.head
                    [ white
                    , Options.scrim 0.75
                    , css "padding" "16px" -- Restore default padding inside scrim
                    , css "width" "100%"
                    ]
                    [ text (persona.name ++ " " ++ (toString (Personas.initiative persona))) ]
                ]
            , Card.actions
                [ Card.border ]
                [ Button.render Mdl [1,0] model.mdl
                    [
                    Button.ripple,
                    Button.accent,
                    Options.onClick Previous
                    ]
                    [ text "Previous" ]
                , Button.render Mdl [1,1] model.mdl
                    [
                    Button.ripple,
                    Button.accent,
                    Options.onClick Remove
                    ]
                    [ text "Remove" ]
                , Button.render Mdl [1,2] model.mdl
                    [
                    Button.ripple,
                    Button.accent,
                    Options.onClick Next
                    ]
                    [ text "Next" ]
                ]
            ]

getImagePathForName : String -> String
getImagePathForName name =
  if List.member name imagefulNames
  then "assets/" ++ (String.toLower (String.join "_" (String.words name))) ++ ".jpg"
  else "assets/rival.png"

imagefulNames : List String
imagefulNames =
  ["Valanthe", "Felix Panafix", "Quallan", "Dien"]

viewQueue : List Persona -> Html Msg
viewQueue personas =
  Table.table
  [
    css "width" "400px",
    css "margin" "auto"
  ]
  [ Table.thead []
    [ Table.tr []
      [ Table.th [] [ text "Character" ]
      , Table.th [] [ text "Initiative" ]
      ]
    ]
  , Table.tbody []
      (personas |> List.map (\persona ->
         Table.tr []
           [ Table.td [css "width" "20%"] [ text persona.name ]
           , Table.td [css "width" "40%"] [ text (Personas.initiative persona |> toString) ]
           ]
         )
      )
  ]

viewAddPersona : Model -> Html Msg
viewAddPersona model =
  Button.render Mdl [0] model.mdl
    [
    Button.fab,
    Button.colored,
    Dialog.openOn "click",
    css "position" "fixed",
    css "right" "80px",
    css "bottom" "80px"
    ]
    [ Icon.i "add"]

dialog : Model -> Html Msg
dialog model =
    Dialog.view
        [ css "position" "fixed",
          css "top" "120px",
          css "left" "0",
          css "max-width" "100%",
          css "max-height" "100%",
          css "z-index" "10"
          ]
        [ Dialog.title [] [ text "Add character" ]
        , Dialog.content []
            [ Textfield.render Mdl [4] model.mdl
                [
                Textfield.floatingLabel,
                Textfield.label "Name",
                Textfield.value model.toBeAdded.name,
                Options.onInput NewName,
                Textfield.error "Name cannot be empty"
                    |> Options.when (String.isEmpty model.toBeAdded.name)
                ] [],
              Textfield.render Mdl [5] model.mdl
                [
                Textfield.floatingLabel,
                Textfield.label "Value",
                Textfield.value <| convertMaybeIntToString model.toBeAdded.value,
                Options.onInput NewValue,
--                Textfield.error "This must be an integer"
--                    |> Options.when (case (convertMaybeIntToString model.toBeAdded.value) of
--                      "" -> True
--                      _ -> False),
                css "margin-top" "8px"
                ] [],
              Textfield.render Mdl [6] model.mdl
                [
                Textfield.floatingLabel,
                Textfield.label "Modifier",
                Textfield.value <| convertMaybeIntToString model.toBeAdded.modifier,
                Options.onInput NewModifier,
--                Textfield.error "This must be an integer"
--                    |> Options.when (case (convertMaybeIntToString model.toBeAdded.modifier) of
--                      "" -> True
--                      _ -> False),
                css "margin-top" "8px"
                ] [],
              Toggles.checkbox Mdl [7] model.mdl
                [
                Options.onToggle ToggleFriendly,
                Toggles.ripple,
                Toggles.value model.toBeAdded.friendly,
                css "margin-top" "12px"
                ]
                [ text "Friendly" ]

            ]
        , Dialog.actions []
            [ Button.render Mdl
                [ 10 ]
                model.mdl
                [ Dialog.closeOn "click",
                  Options.onClick AddPersona ]
                [ text "Add" ]
            , Button.render Mdl
                [ 11 ]
                model.mdl
                [ Dialog.closeOn "click"]
                [ text "Close" ]
            ]
        ]
viewResetQueue : Model -> Html Msg
viewResetQueue model =
  Button.render Mdl [0] model.mdl
    [
    Button.fab,
    Button.colored,
    Options.onClick Reset,
    css "position" "fixed",
    css "left" "80px",
    css "bottom" "80px"
    ]
    [ Icon.i "clear"]