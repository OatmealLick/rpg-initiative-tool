module Personas exposing (Persona, sortPersonas, initiative)

type alias Persona =
  {
    name : String,
    value : Maybe Int,
    modifier : Maybe Int,
    friendly : Bool
  }

sortPersonas : List Persona -> List Persona
sortPersonas personas =
  List.sortWith initiativeOrder personas

initiativeOrder : Persona -> Persona -> Order
initiativeOrder persona1 persona2 =
  let
    initiative1 = (extractValue persona1.value) + (extractValue persona1.modifier)
    initiative2 = (extractValue persona2.value) + (extractValue persona2.modifier)
  in
    if initiative1 == initiative2
    then EQ
    else
      case initiative1 > initiative2 of
        True -> LT
        False -> GT

initiative : Persona -> Int
initiative persona =
  (extractValue persona.value) + (extractValue persona.modifier)

extractValue : Maybe Int -> Int
extractValue maybeValue =
  case maybeValue of
    Nothing -> 0
    Just value -> value