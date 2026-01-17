module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode
import Json.Encode as Encode


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( initialModel, Cmd.none )
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }



-- MODEL


{-| Oklch color space - perceptually uniform
    L: Lightness (0-1)
    C: Chroma (0-0.4, typically 0-0.37 for sRGB)
    H: Hue (0-360)
-}
type alias Oklch =
    { lightness : Float -- 0-1
    , chroma : Float -- 0-0.4
    , hue : Float -- 0-360
    }


type alias ColorScale =
    { name : String
    , shades : List ( Int, Oklch ) -- (level, color)
    }


type Step
    = PickBase
    | Done


type alias ShadeEditor =
    { scaleName : String
    , level : Int
    , color : Oklch
    }


type alias Model =
    { scales : List ColorScale
    , currentScale : Int
    , step : Step
    , workingColor : Oklch
    , shadeEditor : Maybe ShadeEditor
    , highlightedColors : List String -- List of hex colors to highlight in palette
    , useSnakeCase : Bool
    }


shadeLevels : List Int
shadeLevels =
    [ 0, 50, 100, 200, 300, 400, 500, 600, 700, 800, 900, 950 ]


emptyScale : String -> ColorScale
emptyScale name =
    { name = name
    , shades = []
    }


initialModel : Model
initialModel =
    { scales =
        [ emptyScale "gray"
        , emptyScale "primary"
        ]
    , currentScale = 1 -- Start with primary
    , step = PickBase
    , workingColor = { lightness = 0.6, chroma = 0.15, hue = 210 }
    , shadeEditor = Nothing
    , highlightedColors = []
    , useSnakeCase = False
    }


getShade : Int -> ColorScale -> Maybe Oklch
getShade level scale =
    List.filter (\( l, _ ) -> l == level) scale.shades
        |> List.head
        |> Maybe.map Tuple.second


getCurrentScale : Model -> Maybe ColorScale
getCurrentScale model =
    List.drop model.currentScale model.scales |> List.head



-- UPDATE


type Msg
    = SetLightness Float
    | SetChroma Float
    | SetHue Float
    | ConfirmColor
    | SelectScale Int
    | AddScale
    | RemoveScale Int
    | RenameScale Int String
    | Reset
    | OpenShadeEditor String Int Oklch
    | CloseShadeEditor
    | SetEditorLightness Float
    | SetEditorChroma Float
    | SetEditorHue Float
    | SaveShadeEdit
    | HighlightColors (List String)
    | ClearHighlight
    | ToggleSnakeCase
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetLightness l ->
            let
                wc =
                    model.workingColor
            in
            ( { model | workingColor = { wc | lightness = l } }, Cmd.none )

        SetChroma c ->
            let
                wc =
                    model.workingColor
            in
            ( { model | workingColor = { wc | chroma = c } }, Cmd.none )

        SetHue h ->
            let
                wc =
                    model.workingColor
            in
            ( { model | workingColor = { wc | hue = h } }, Cmd.none )

        ConfirmColor ->
            case model.step of
                PickBase ->
                    let
                        baseColor =
                            model.workingColor

                        -- Generate all 11 shades from the base color
                        generatedShades =
                            generateFullScale baseColor

                        -- Get current scale to check if it's gray or primary
                        currentScaleName =
                            List.drop model.currentScale model.scales
                                |> List.head
                                |> Maybe.map .name
                                |> Maybe.withDefault ""

                        -- Update current scale with generated shades
                        newScales =
                            List.indexedMap
                                (\i scale ->
                                    if i == model.currentScale then
                                        { scale | shades = generatedShades }

                                    else
                                        scale
                                )
                                model.scales

                        -- If this is the primary color, also generate semantic colors
                        finalScales =
                            if currentScaleName == "primary" then
                                newScales ++ generateSemanticColors baseColor

                            else
                                newScales
                    in
                    ( { model
                        | scales = finalScales
                        , step = Done
                      }
                    , Cmd.none
                    )

                Done ->
                    ( model, Cmd.none )

        SelectScale i ->
            let
                scale =
                    List.drop i model.scales |> List.head

                suggestedHue =
                    case scale of
                        Just s ->
                            getDefaultHueForName s.name

                        Nothing ->
                            210

                ( step, workingColor ) =
                    case scale of
                        Just s ->
                            if List.isEmpty s.shades then
                                ( PickBase, { lightness = 0.6, chroma = (if s.name == "gray" then 0.02 else 0.15), hue = suggestedHue } )

                            else
                                ( Done, model.workingColor )

                        Nothing ->
                            ( PickBase, model.workingColor )
            in
            ( { model | currentScale = i, step = step, workingColor = workingColor }, Cmd.none )

        AddScale ->
            let
                newName =
                    "accent-" ++ String.fromInt (List.length model.scales - 1)
            in
            ( { model | scales = model.scales ++ [ emptyScale newName ] }, Cmd.none )

        RemoveScale i ->
            if List.length model.scales > 1 then
                ( { model
                    | scales = List.take i model.scales ++ List.drop (i + 1) model.scales
                    , currentScale =
                        if model.currentScale >= i && model.currentScale > 0 then
                            model.currentScale - 1

                        else
                            model.currentScale
                  }
                , Cmd.none
                )

            else
                ( model, Cmd.none )

        RenameScale i newName ->
            ( { model
                | scales =
                    List.indexedMap
                        (\idx scale ->
                            if idx == i then
                                { scale | name = newName }

                            else
                                scale
                        )
                        model.scales
              }
            , Cmd.none
            )

        Reset ->
            let
                currentScaleName =
                    List.drop model.currentScale model.scales
                        |> List.head
                        |> Maybe.map .name
                        |> Maybe.withDefault ""

                suggestedHue =
                    getDefaultHueForName currentScaleName

                -- If resetting primary, also remove semantic colors
                newScales =
                    if currentScaleName == "primary" then
                        List.filter (\s -> not (List.member s.name [ "danger", "warning", "success" ])) model.scales
                            |> List.indexedMap
                                (\i scale ->
                                    if scale.name == "primary" then
                                        { scale | shades = [] }

                                    else
                                        scale
                                )

                    else
                        List.indexedMap
                            (\i scale ->
                                if i == model.currentScale then
                                    { scale | shades = [] }

                                else
                                    scale
                            )
                            model.scales
            in
            ( { model
                | scales = newScales
                , step = PickBase
                , workingColor = { lightness = 0.6, chroma = (if currentScaleName == "gray" then 0.02 else 0.15), hue = suggestedHue }
              }
            , Cmd.none
            )

        OpenShadeEditor scaleName level color ->
            ( { model
                | shadeEditor =
                    Just
                        { scaleName = scaleName
                        , level = level
                        , color = color
                        }
              }
            , Cmd.none
            )

        CloseShadeEditor ->
            ( { model | shadeEditor = Nothing }, Cmd.none )

        SetEditorLightness l ->
            case model.shadeEditor of
                Just editor ->
                    let
                        c =
                            editor.color
                    in
                    ( { model | shadeEditor = Just { editor | color = { c | lightness = l } } }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        SetEditorChroma c ->
            case model.shadeEditor of
                Just editor ->
                    let
                        color =
                            editor.color
                    in
                    ( { model | shadeEditor = Just { editor | color = { color | chroma = c } } }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        SetEditorHue h ->
            case model.shadeEditor of
                Just editor ->
                    let
                        c =
                            editor.color
                    in
                    ( { model | shadeEditor = Just { editor | color = { c | hue = h } } }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        SaveShadeEdit ->
            case model.shadeEditor of
                Just editor ->
                    let
                        newScales =
                            List.map
                                (\scale ->
                                    if scale.name == editor.scaleName then
                                        { scale | shades = setShade editor.level editor.color scale.shades }

                                    else
                                        scale
                                )
                                model.scales
                    in
                    ( { model | scales = newScales, shadeEditor = Nothing }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        HighlightColors colors ->
            ( { model | highlightedColors = colors }, Cmd.none )

        ClearHighlight ->
            ( { model | highlightedColors = [] }, Cmd.none )

        ToggleSnakeCase ->
            ( { model | useSnakeCase = not model.useSnakeCase }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


getCurrentScaleShades : List ColorScale -> Int -> List ( Int, Oklch )
getCurrentScaleShades scales idx =
    List.drop idx scales
        |> List.head
        |> Maybe.map .shades
        |> Maybe.withDefault []


getDefaultHueForName : String -> Float
getDefaultHueForName name =
    case String.toLower name of
        "gray" ->
            220

        "grey" ->
            220

        "primary" ->
            210

        "red" ->
            0

        "orange" ->
            25

        "yellow" ->
            45

        "green" ->
            145

        "teal" ->
            175

        "cyan" ->
            190

        "blue" ->
            210

        "indigo" ->
            235

        "purple" ->
            270

        "pink" ->
            330

        _ ->
            210


setShade : Int -> Oklch -> List ( Int, Oklch ) -> List ( Int, Oklch )
setShade level color shades =
    let
        filtered =
            List.filter (\( l, _ ) -> l /= level) shades
    in
    ( level, color ) :: filtered |> List.sortBy Tuple.first


{-| Generate all 11 shades from a base color (500)
-}
generateFullScale : Oklch -> List ( Int, Oklch )
generateFullScale base =
    let
        -- Reference lightness values (when base is 0.6)
        -- We'll scale these relative to the actual base lightness
        referenceLightness =
            0.6

        -- How much room we have to go lighter/darker
        lighterRoom =
            1.0 - base.lightness

        darkerRoom =
            base.lightness - 0.15

        -- Scale factor for lighter shades (50-400)
        lighterScale =
            lighterRoom / (1.0 - referenceLightness)

        -- Scale factor for darker shades (600-950)
        darkerScale =
            darkerRoom / (referenceLightness - 0.15)

        -- Map shade level to target lightness, scaled relative to base
        lightnessFor level =
            if level == 0 then
                -- Pure white
                1.0

            else if level == 500 then
                base.lightness

            else if level < 500 then
                -- Lighter shades: interpolate from base to near-white
                let
                    -- How far from 500 toward 50 (0 = at 500, 1 = at 50)
                    t =
                        toFloat (500 - level) / 450

                    -- Reference offset from 0.6
                    refOffset =
                        t * 0.37
                in
                Basics.min 0.98 (base.lightness + refOffset * lighterScale)

            else
                -- Darker shades: interpolate from base to near-black
                let
                    -- How far from 500 toward 950 (0 = at 500, 1 = at 950)
                    t =
                        toFloat (level - 500) / 450

                    -- Reference offset from 0.6
                    refOffset =
                        t * 0.38
                in
                Basics.max 0.15 (base.lightness - refOffset * darkerScale)

        -- Chroma curve: lower at extremes, peaks around base
        chromaFor level =
            if level == 0 then
                -- Pure white has no chroma
                0

            else
                let
                    -- Normalize level to 0-1 range (50 -> 0, 950 -> 1)
                    t =
                        (toFloat level - 50) / 900

                    -- Bell curve peaking at ~0.5 (around 500)
                    curve =
                        1 - (2 * t - 1) ^ 2
                in
                base.chroma * (0.3 + 0.7 * curve)

        makeShade level =
            ( level
            , { lightness = lightnessFor level
              , chroma = chromaFor level
              , hue = base.hue
              }
            )
    in
    List.map makeShade shadeLevels


{-| Generate semantic colors (red, yellow, green) with 3 shades each
-}
generateSemanticColors : Oklch -> List ColorScale
generateSemanticColors primary =
    let
        makeSemanticScale name hue baseLevel baseLightness =
            { name = name
            , shades =
                [ -- Light (for backgrounds)
                  ( 100
                  , { lightness = 0.93
                    , chroma = primary.chroma * 0.4
                    , hue = hue
                    }
                  )

                -- Base (for buttons, icons)
                , ( baseLevel
                  , { lightness = baseLightness
                    , chroma = primary.chroma
                    , hue = hue
                    }
                  )

                -- Dark (for text)
                , ( 700
                  , { lightness = 0.40
                    , chroma = primary.chroma * 0.8
                    , hue = hue
                    }
                  )
                ]
            }
    in
    [ makeSemanticScale "danger" 25 500 primary.lightness -- Red
    , makeSemanticScale "warning" 85 200 0.87 -- Yellow uses 200 (lighter) because 500 looks bad
    , makeSemanticScale "success" 145 500 primary.lightness -- Green
    ]



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "app" ]
        [ viewHeader
        , div [ class "main-content" ]
            [ div [ class "top-row" ]
                [ div [ class "picker-panel" ]
                    [ viewWorkflow model ]
                , div [ class "palette-panel" ]
                    [ viewLivePreview model ]
                ]
            , div [ class "previews-row" ]
                [ div [ class "preview-section" ]
                    [ h3 [ class "preview-label" ] [ text "Light Mode" ]
                    , viewChatPreview model
                    , viewButtonPreview model False
                    ]
                , div [ class "preview-section" ]
                    [ h3 [ class "preview-label" ] [ text "Dark Mode" ]
                    , viewChatPreviewDark model
                    , viewButtonPreview model True
                    ]
                ]
            ]
        , viewExport model
        , viewShadeEditorPopup model
        ]


viewHeader : Html Msg
viewHeader =
    header [ class "header" ]
        [ h1 [] [ text "Palette Builder" ]
        , p [ class "subtitle" ]
            [ text "Build your color palette based on "
            , a [ href "https://www.refactoringui.com/previews/building-your-color-palette", target "_blank" ]
                [ text "Refactoring UI" ]
            ]
        ]


viewScaleSelector : Model -> Html Msg
viewScaleSelector model =
    div [ class "scale-selector" ]
        [ h3 [] [ text "Color Scales" ]
        , div [ class "scale-list" ]
            (List.indexedMap (viewScaleTab model.currentScale) model.scales
                ++ [ button [ class "add-scale-btn", onClick AddScale ] [ text "+ Add" ] ]
            )
        ]


viewScaleTab : Int -> Int -> ColorScale -> Html Msg
viewScaleTab currentIdx idx scale =
    div
        [ class
            ("scale-tab"
                ++ (if currentIdx == idx then
                        " active"

                    else
                        ""
                   )
            )
        , onClick (SelectScale idx)
        ]
        [ input
            [ type_ "text"
            , value scale.name
            , onInput (RenameScale idx)
            , stopPropagationOn "click" (Decode.succeed ( SelectScale idx, True ))
            , class "scale-name-input"
            ]
            []
        , if List.length scale.shades > 0 then
            span [ class "shade-count" ] [ text (String.fromInt (List.length scale.shades) ++ "/11") ]

          else
            text ""
        , if idx > 1 then
            button
                [ class "remove-scale-btn"
                , stopPropagationOn "click" (Decode.succeed ( RemoveScale idx, True ))
                ]
                [ text "×" ]

          else
            text ""
        ]


viewWorkflow : Model -> Html Msg
viewWorkflow model =
    case model.step of
        Done ->
            div [ class "workflow done" ]
                [ h2 [] [ text "Scale Complete!" ]
                , p [] [ text "All 11 shades have been defined. You can select another scale or add a new one." ]
                , button [ class "reset-btn", onClick Reset ] [ text "Reset This Scale" ]
                ]

        _ ->
            div [ class "workflow" ]
                [ viewStepHeader model.step
                , viewStepHint model
                , viewColorPicker model
                ]


viewStepHeader : Step -> Html Msg
viewStepHeader step =
    let
        title =
            case step of
                PickBase ->
                    "Pick your base color"

                Done ->
                    "Complete"
    in
    div [ class "step-header" ]
        [ h2 [] [ text title ]
        ]


viewStepHint : Model -> Html Msg
viewStepHint model =
    let
        currentScaleName =
            getCurrentScale model
                |> Maybe.map .name
                |> Maybe.withDefault ""

        hint =
            case model.step of
                PickBase ->
                    if currentScaleName == "primary" then
                        "Choose your primary brand color. All shades will be generated automatically, plus danger/warning/success colors."

                    else if currentScaleName == "gray" then
                        "Choose your base gray. A slight tint (low chroma) can add warmth or coolness to your palette."

                    else
                        "Choose a base color for this scale."

                Done ->
                    "Click any shade to fine-tune it."
    in
    if String.isEmpty hint then
        text ""

    else
        p [ class "step-hint" ] [ text hint ]


viewColorPicker : Model -> Html Msg
viewColorPicker model =
    let
        oklch =
            model.workingColor

        hexColor =
            oklchToHex oklch
    in
    div [ class "color-picker-section" ]
        [ div [ class "color-preview-large", style "background-color" hexColor ]
            [ span
                [ class "preview-hex"
                , style "color"
                    (if oklch.lightness > 0.6 then
                        "#000"

                     else
                        "#fff"
                    )
                ]
                [ text hexColor ]
            ]
        , div [ class "sliders" ]
            [ viewSlider "Lightness" 0 1 oklch.lightness SetLightness (lightnessGradient oklch)
            , viewSlider "Chroma" 0 0.4 oklch.chroma SetChroma (chromaGradient oklch)
            , viewSlider "Hue" 0 360 oklch.hue SetHue (hueGradient oklch)
            ]
        , button [ class "confirm-btn", onClick ConfirmColor ] [ text "Confirm & Continue" ]
        ]


viewSlider : String -> Float -> Float -> Float -> (Float -> Msg) -> String -> Html Msg
viewSlider labelText minVal maxVal currentVal toMsg gradient =
    let
        displayValue =
            if maxVal <= 1 then
                String.fromFloat (toFloat (round (currentVal * 100)) / 100)

            else
                String.fromInt (round currentVal)
    in
    div [ class "slider-row" ]
        [ div [ class "slider-label" ]
            [ text labelText
            , span [ class "slider-value" ] [ text displayValue ]
            ]
        , div [ class "slider-track", style "background" gradient ]
            [ input
                [ type_ "range"
                , Html.Attributes.min (String.fromFloat minVal)
                , Html.Attributes.max (String.fromFloat maxVal)
                , value (String.fromFloat currentVal)
                , step (if maxVal <= 1 then "0.01" else "1")
                , onInput (\s -> String.toFloat s |> Maybe.withDefault currentVal |> toMsg)
                , class "slider-input"
                ]
                []
            ]
        ]


lightnessGradient : Oklch -> String
lightnessGradient oklch =
    let
        stops =
            List.map
                (\l -> oklchToHex { oklch | lightness = toFloat l / 100 })
                [ 0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100 ]
    in
    "linear-gradient(to right, " ++ String.join ", " stops ++ ")"


chromaGradient : Oklch -> String
chromaGradient oklch =
    let
        stops =
            List.map
                (\c -> oklchToHex { oklch | chroma = toFloat c / 1000 })
                [ 0, 50, 100, 150, 200, 250, 300, 350, 400 ]
    in
    "linear-gradient(to right, " ++ String.join ", " stops ++ ")"


hueGradient : Oklch -> String
hueGradient oklch =
    let
        stops =
            List.map
                (\h -> oklchToHex { oklch | hue = toFloat h })
                (List.range 0 36 |> List.map (\i -> i * 10))
    in
    "linear-gradient(to right, " ++ String.join ", " stops ++ ")"


viewLivePreview : Model -> Html Msg
viewLivePreview model =
    let
        currentScaleName =
            getCurrentScale model
                |> Maybe.map .name
                |> Maybe.withDefault ""

        -- Show live preview when picking base color
        previewScales =
            case model.step of
                PickBase ->
                    if currentScaleName == "primary" then
                        let
                            grayBase =
                                { lightness = 0.6, chroma = 0.02, hue = model.workingColor.hue }
                        in
                        [ ( "gray", generateFullScale grayBase )
                        , ( "primary", generateFullScale model.workingColor )
                        ]
                            ++ List.map (\s -> ( s.name, s.shades )) (generateSemanticColors model.workingColor)

                    else if currentScaleName == "gray" then
                        [ ( "gray", generateFullScale model.workingColor ) ]

                    else
                        [ ( currentScaleName, generateFullScale model.workingColor ) ]

                Done ->
                    -- Show actual saved scales
                    List.map (\s -> ( s.name, s.shades )) model.scales

        activeLevel =
            case model.step of
                PickBase ->
                    500

                Done ->
                    -1
    in
    div [ class "live-preview" ]
        [ div [ class "palette-scales" ]
            (List.map (viewCompactScale activeLevel (model.step == Done) model.highlightedColors) previewScales)
        , label [ class "snake-case-toggle" ]
            [ input
                [ type_ "checkbox"
                , checked model.useSnakeCase
                , onClick ToggleSnakeCase
                ]
                []
            , text "Use snake_case for CSS names"
            ]
        ]


viewCompactScale : Int -> Bool -> List String -> ( String, List ( Int, Oklch ) ) -> Html Msg
viewCompactScale activeLevel canEdit highlightedColors ( name, shades ) =
    div [ class "compact-scale" ]
        [ span [ class "compact-scale-name" ] [ text name ]
        , div [ class "compact-shades" ]
            (List.map (viewCompactShade name shades activeLevel canEdit highlightedColors) (List.map Tuple.first shades |> List.sort))
        ]


viewCompactShade : String -> List ( Int, Oklch ) -> Int -> Bool -> List String -> Int -> Html Msg
viewCompactShade scaleName shades activeLevel canEdit highlightedColors level =
    let
        maybeOklch =
            List.filter (\( l, _ ) -> l == level) shades
                |> List.head
                |> Maybe.map Tuple.second

        isActive =
            level == activeLevel
    in
    case maybeOklch of
        Just oklch ->
            let
                hexColor =
                    oklchToHex oklch

                isHighlighted =
                    List.member hexColor highlightedColors
            in
            div
                [ class ("compact-shade" ++ activeClass isActive ++ (if canEdit then " clickable" else "") ++ (if isHighlighted then " highlighted" else ""))
                , style "background-color" hexColor
                , if canEdit then onClick (OpenShadeEditor scaleName level oklch) else class ""
                , title (String.fromInt level ++ ": " ++ hexColor)
                ]
                []

        Nothing ->
            text ""


viewShadeEditorPopup : Model -> Html Msg
viewShadeEditorPopup model =
    case model.shadeEditor of
        Nothing ->
            text ""

        Just editor ->
            let
                hexColor =
                    oklchToHex editor.color
            in
            div [ class "shade-editor-overlay", onClick CloseShadeEditor ]
                [ div
                    [ class "shade-editor-popup"
                    , stopPropagationOn "click" (Decode.succeed ( NoOp, True ))
                    ]
                    [ div [ class "popup-header" ]
                        [ span [] [ text (editor.scaleName ++ " " ++ String.fromInt editor.level) ]
                        , button [ class "popup-close", onClick CloseShadeEditor ] [ text "×" ]
                        ]
                    , div
                        [ class "popup-preview"
                        , style "background-color" hexColor
                        ]
                        [ span
                            [ style "color"
                                (if editor.color.lightness > 0.6 then
                                    "#000"

                                 else
                                    "#fff"
                                )
                            ]
                            [ text hexColor ]
                        ]
                    , div [ class "popup-sliders" ]
                        [ viewPopupSlider "L" 0 1 editor.color.lightness SetEditorLightness
                        , viewPopupSlider "C" 0 0.4 editor.color.chroma SetEditorChroma
                        , viewPopupSlider "H" 0 360 editor.color.hue SetEditorHue
                        ]
                    , div [ class "popup-actions" ]
                        [ button [ class "popup-cancel", onClick CloseShadeEditor ] [ text "Cancel" ]
                        , button [ class "popup-save", onClick SaveShadeEdit ] [ text "Save" ]
                        ]
                    ]
                ]


viewPopupSlider : String -> Float -> Float -> Float -> (Float -> Msg) -> Html Msg
viewPopupSlider labelText minVal maxVal currentVal toMsg =
    let
        displayValue =
            if maxVal <= 1 then
                String.fromFloat (toFloat (round (currentVal * 100)) / 100)

            else
                String.fromInt (round currentVal)
    in
    div [ class "popup-slider-row" ]
        [ span [ class "popup-slider-label" ] [ text labelText ]
        , input
            [ type_ "range"
            , Html.Attributes.min (String.fromFloat minVal)
            , Html.Attributes.max (String.fromFloat maxVal)
            , value (String.fromFloat currentVal)
            , step
                (if maxVal <= 1 then
                    "0.01"

                 else
                    "1"
                )
            , onInput (\s -> String.toFloat s |> Maybe.withDefault currentVal |> toMsg)
            , class "popup-slider"
            ]
            []
        , span [ class "popup-slider-value" ] [ text displayValue ]
        ]


activeClass : Bool -> String
activeClass isActive =
    if isActive then
        " active"

    else
        ""


viewChatPreview : Model -> Html Msg
viewChatPreview model =
    let
        colors =
            getPreviewColors model

        clickable colorList attrs children =
            div ([ class "preview-clickable", stopPropagationOn "click" (Decode.succeed ( HighlightColors colorList, True )) ] ++ attrs) children
    in
    div [ class "chat-preview", onClick ClearHighlight ]
        [ div
            [ class "chat-sidebar"
            , style "background-color" colors.primary800
            , stopPropagationOn "click" (Decode.succeed ( HighlightColors [ colors.primary800 ], True ))
            ]
            [ clickable [ colors.primary800, colors.gray0 ]
                [ class "chat-workspace" ]
                [ span [ style "color" colors.gray0 ] [ text "Workspace" ]
                , div [ class "chat-user" ]
                    [ span [ class "status-dot preview-clickable", style "background-color" colors.success500, stopPropagationOn "click" (Decode.succeed ( HighlightColors [ colors.success500 ], True )) ] []
                    , span [ style "color" colors.primary200 ] [ text "You" ]
                    ]
                ]
            , div [ class "chat-nav" ]
                [ clickable [ colors.primary800, colors.primary100 ]
                    [ class "chat-nav-item", style "color" colors.primary100 ]
                    [ text "Inbox" ]
                , clickable [ colors.primary800, colors.primary100 ]
                    [ class "chat-nav-item", style "color" colors.primary100 ]
                    [ text "Starred" ]
                ]
            , clickable [ colors.primary800, colors.primary300 ]
                [ class "chat-channels-label", style "color" colors.primary300 ]
                [ text "CHANNELS" ]
            , div [ class "chat-channels" ]
                [ clickable [ colors.primary900, colors.gray0 ]
                    [ class "chat-channel selected"
                    , style "background-color" colors.primary900
                    , style "color" colors.gray0
                    ]
                    [ text "# design" ]
                , clickable [ colors.primary800, colors.primary100 ]
                    [ class "chat-channel"
                    , style "color" colors.primary100
                    ]
                    [ text "# engineering"
                    , span
                        [ class "unread-badge preview-clickable"
                        , style "background-color" colors.danger500
                        , style "color" colors.gray0
                        , stopPropagationOn "click" (Decode.succeed ( HighlightColors [ colors.danger500, colors.gray0 ], True ))
                        ]
                        [ text "4" ]
                    ]
                , clickable [ colors.primary800, colors.primary100 ]
                    [ class "chat-channel", style "color" colors.primary100 ]
                    [ text "# marketing" ]
                ]
            ]
        , div
            [ class "chat-main"
            , style "background-color" colors.gray0
            , stopPropagationOn "click" (Decode.succeed ( HighlightColors [ colors.gray0 ], True ))
            ]
            [ clickable [ colors.gray0, colors.gray900 ]
                [ class "chat-header", style "border-color" colors.gray200 ]
                [ span [ style "color" colors.gray900 ] [ text "# design" ] ]
            , div [ class "chat-alerts" ]
                [ clickable [ colors.danger100, colors.danger700 ]
                    [ class "chat-alert"
                    , style "background-color" colors.danger100
                    , style "border-color" colors.danger200
                    ]
                    [ div [ class "alert-icon", style "color" colors.danger700 ] [ text "✕" ]
                    , div [ style "color" colors.danger700 ] [ text "Upload failed. Please try again." ]
                    ]
                , clickable [ colors.warning100, colors.warning700 ]
                    [ class "chat-alert"
                    , style "background-color" colors.warning100
                    , style "border-color" colors.warning200
                    ]
                    [ div [ class "alert-icon", style "color" colors.warning700 ] [ text "!" ]
                    , div [ style "color" colors.warning700 ] [ text "Connection unstable." ]
                    ]
                , clickable [ colors.success100, colors.success700 ]
                    [ class "chat-alert"
                    , style "background-color" colors.success100
                    , style "border-color" colors.success200
                    ]
                    [ div [ class "alert-icon", style "color" colors.success700 ] [ text "✓" ]
                    , div [ style "color" colors.success700 ] [ text "Message sent successfully." ]
                    ]
                ]
            , div [ class "chat-messages" ]
                [ clickable [ colors.gray0, colors.gray700 ]
                    [ class "chat-message" ]
                    [ div [ class "message-avatar", style "background-color" colors.primary400 ] []
                    , div [ class "message-content" ]
                        [ div [ class "message-header" ]
                            [ span [ class "message-author", style "color" colors.gray900 ] [ text "Sarah Porter" ]
                            , span [ class "message-time", style "color" colors.gray400 ] [ text "12:48 PM" ]
                            ]
                        , div [ class "message-text", style "color" colors.gray700 ]
                            [ text "No problem! I'll upload the notes shortly." ]
                        ]
                    ]
                , clickable [ colors.gray0, colors.gray700 ]
                    [ class "chat-message" ]
                    [ div [ class "message-avatar", style "background-color" colors.gray400 ] []
                    , div [ class "message-content" ]
                        [ div [ class "message-header" ]
                            [ span [ class "message-author", style "color" colors.gray900 ] [ text "Tiffany Myers" ]
                            , span [ class "message-time", style "color" colors.gray400 ] [ text "12:51 PM" ]
                            ]
                        , div [ class "message-text", style "color" colors.gray700 ]
                            [ span
                                [ style "color" colors.primary500
                                , class "preview-clickable"
                                , stopPropagationOn "click" (Decode.succeed ( HighlightColors [ colors.primary500 ], True ))
                                ]
                                [ text "@sarah " ]
                            , text "I put the photos in the shared folder."
                            ]
                        ]
                    ]
                ]
            , clickable [ colors.gray50, colors.gray400 ]
                [ class "chat-input", style "background-color" colors.gray50, style "border-color" colors.gray200 ]
                [ span [ style "color" colors.gray400 ] [ text "Type your message..." ] ]
            ]
        ]


viewChatPreviewDark : Model -> Html Msg
viewChatPreviewDark model =
    let
        colors =
            getPreviewColors model

        clickable colorList attrs children =
            div ([ class "preview-clickable", stopPropagationOn "click" (Decode.succeed ( HighlightColors colorList, True )) ] ++ attrs) children
    in
    div [ class "chat-preview dark", onClick ClearHighlight ]
        [ div
            [ class "chat-sidebar"
            , style "background-color" colors.primary800
            , stopPropagationOn "click" (Decode.succeed ( HighlightColors [ colors.primary800 ], True ))
            ]
            [ clickable [ colors.primary800, colors.gray0 ]
                [ class "chat-workspace" ]
                [ span [ style "color" colors.gray0 ] [ text "Workspace" ]
                , div [ class "chat-user" ]
                    [ span [ class "status-dot preview-clickable", style "background-color" colors.success500, stopPropagationOn "click" (Decode.succeed ( HighlightColors [ colors.success500 ], True )) ] []
                    , span [ style "color" colors.primary200 ] [ text "You" ]
                    ]
                ]
            , div [ class "chat-nav" ]
                [ clickable [ colors.primary800, colors.primary100 ]
                    [ class "chat-nav-item", style "color" colors.primary100 ]
                    [ text "Inbox" ]
                , clickable [ colors.primary800, colors.primary100 ]
                    [ class "chat-nav-item", style "color" colors.primary100 ]
                    [ text "Starred" ]
                ]
            , clickable [ colors.primary800, colors.primary300 ]
                [ class "chat-channels-label", style "color" colors.primary300 ]
                [ text "CHANNELS" ]
            , div [ class "chat-channels" ]
                [ clickable [ colors.primary900, colors.gray0 ]
                    [ class "chat-channel selected"
                    , style "background-color" colors.primary900
                    , style "color" colors.gray0
                    ]
                    [ text "# design" ]
                , clickable [ colors.primary800, colors.primary100 ]
                    [ class "chat-channel"
                    , style "color" colors.primary100
                    ]
                    [ text "# engineering"
                    , span
                        [ class "unread-badge preview-clickable"
                        , style "background-color" colors.danger500
                        , style "color" colors.gray0
                        , stopPropagationOn "click" (Decode.succeed ( HighlightColors [ colors.danger500, colors.gray0 ], True ))
                        ]
                        [ text "4" ]
                    ]
                , clickable [ colors.primary800, colors.primary100 ]
                    [ class "chat-channel", style "color" colors.primary100 ]
                    [ text "# marketing" ]
                ]
            ]
        , div
            [ class "chat-main"
            , style "background-color" colors.gray900
            , stopPropagationOn "click" (Decode.succeed ( HighlightColors [ colors.gray900 ], True ))
            ]
            [ clickable [ colors.gray900, colors.gray100 ]
                [ class "chat-header", style "border-color" colors.gray700 ]
                [ span [ style "color" colors.gray100 ] [ text "# design" ] ]
            , div [ class "chat-alerts" ]
                [ clickable [ colors.danger900, colors.danger200 ]
                    [ class "chat-alert"
                    , style "background-color" colors.danger900
                    , style "border-color" colors.danger800
                    ]
                    [ div [ class "alert-icon", style "color" colors.danger200 ] [ text "✕" ]
                    , div [ style "color" colors.danger200 ] [ text "Upload failed. Please try again." ]
                    ]
                , clickable [ colors.warning900, colors.warning200 ]
                    [ class "chat-alert"
                    , style "background-color" colors.warning900
                    , style "border-color" colors.warning800
                    ]
                    [ div [ class "alert-icon", style "color" colors.warning200 ] [ text "!" ]
                    , div [ style "color" colors.warning200 ] [ text "Connection unstable." ]
                    ]
                , clickable [ colors.success900, colors.success200 ]
                    [ class "chat-alert"
                    , style "background-color" colors.success900
                    , style "border-color" colors.success800
                    ]
                    [ div [ class "alert-icon", style "color" colors.success200 ] [ text "✓" ]
                    , div [ style "color" colors.success200 ] [ text "Message sent successfully." ]
                    ]
                ]
            , div [ class "chat-messages" ]
                [ clickable [ colors.gray900, colors.gray300 ]
                    [ class "chat-message" ]
                    [ div [ class "message-avatar", style "background-color" colors.primary400 ] []
                    , div [ class "message-content" ]
                        [ div [ class "message-header" ]
                            [ span [ class "message-author", style "color" colors.gray100 ] [ text "Sarah Porter" ]
                            , span [ class "message-time", style "color" colors.gray500 ] [ text "12:48 PM" ]
                            ]
                        , div [ class "message-text", style "color" colors.gray300 ]
                            [ text "No problem! I'll upload the notes shortly." ]
                        ]
                    ]
                , clickable [ colors.gray900, colors.gray300 ]
                    [ class "chat-message" ]
                    [ div [ class "message-avatar", style "background-color" colors.gray600 ] []
                    , div [ class "message-content" ]
                        [ div [ class "message-header" ]
                            [ span [ class "message-author", style "color" colors.gray100 ] [ text "Tiffany Myers" ]
                            , span [ class "message-time", style "color" colors.gray500 ] [ text "12:51 PM" ]
                            ]
                        , div [ class "message-text", style "color" colors.gray300 ]
                            [ span
                                [ style "color" colors.primary400
                                , class "preview-clickable"
                                , stopPropagationOn "click" (Decode.succeed ( HighlightColors [ colors.primary400 ], True ))
                                ]
                                [ text "@sarah " ]
                            , text "I put the photos in the shared folder."
                            ]
                        ]
                    ]
                ]
            , clickable [ colors.gray800, colors.gray500 ]
                [ class "chat-input", style "background-color" colors.gray800, style "border-color" colors.gray700 ]
                [ span [ style "color" colors.gray500 ] [ text "Type your message..." ] ]
            ]
        ]


viewButtonPreview : Model -> Bool -> Html Msg
viewButtonPreview model isDark =
    let
        colors =
            getPreviewColors model

        bgColor =
            if isDark then
                colors.gray900
            else
                colors.gray0

        secondaryBg =
            if isDark then
                colors.gray700
            else
                colors.gray100

        secondaryText =
            if isDark then
                colors.gray100
            else
                colors.gray700

        secondaryBorder =
            if isDark then
                colors.gray600
            else
                colors.gray300
    in
    div
        [ class "button-preview"
        , style "background-color" bgColor
        , style "padding" "1rem"
        , style "border-radius" "8px"
        , style "margin-top" "0.5rem"
        ]
        [ div [ class "button-row" ]
            [ button
                [ class "preview-btn"
                , style "background-color" colors.primary500
                , style "color" colors.gray0
                ]
                [ text "Primary" ]
            , button
                [ class "preview-btn"
                , style "background-color" secondaryBg
                , style "color" secondaryText
                , style "border" ("1px solid " ++ secondaryBorder)
                ]
                [ text "Secondary" ]
            , button
                [ class "preview-btn"
                , style "background-color" colors.danger500
                , style "color" colors.gray0
                ]
                [ text "Danger" ]
            , button
                [ class "preview-btn"
                , style "background-color" colors.warning200
                , style "color" colors.warning800
                ]
                [ text "Warning" ]
            , button
                [ class "preview-btn"
                , style "background-color" colors.success500
                , style "color" colors.gray0
                ]
                [ text "Success" ]
            ]
        ]


type alias PreviewColors =
    { primary100 : String
    , primary200 : String
    , primary300 : String
    , primary400 : String
    , primary500 : String
    , primary600 : String
    , primary700 : String
    , primary800 : String
    , primary900 : String
    , gray0 : String
    , gray50 : String
    , gray100 : String
    , gray200 : String
    , gray300 : String
    , gray400 : String
    , gray500 : String
    , gray600 : String
    , gray700 : String
    , gray800 : String
    , gray900 : String
    , gray950 : String
    , danger100 : String
    , danger200 : String
    , danger500 : String
    , danger700 : String
    , danger800 : String
    , danger900 : String
    , warning100 : String
    , warning200 : String
    , warning700 : String
    , warning800 : String
    , warning900 : String
    , success100 : String
    , success200 : String
    , success500 : String
    , success700 : String
    , success800 : String
    , success900 : String
    }


type alias ScaleSet =
    { gray : List ( Int, Oklch )
    , primary : List ( Int, Oklch )
    , danger : List ( Int, Oklch )
    , warning : List ( Int, Oklch )
    , success : List ( Int, Oklch )
    }


getPreviewColors : Model -> PreviewColors
getPreviewColors model =
    let
        currentScaleName =
            getCurrentScale model
                |> Maybe.map .name
                |> Maybe.withDefault ""

        -- Generate preview scales based on current state
        scales : ScaleSet
        scales =
            case model.step of
                PickBase ->
                    if currentScaleName == "primary" then
                        let
                            grayBase =
                                { lightness = 0.6, chroma = 0.02, hue = model.workingColor.hue }

                            semantic =
                                generateSemanticColors model.workingColor
                        in
                        { gray = generateFullScale grayBase
                        , primary = generateFullScale model.workingColor
                        , danger = getScaleShades "danger" semantic
                        , warning = getScaleShades "warning" semantic
                        , success = getScaleShades "success" semantic
                        }

                    else if currentScaleName == "gray" then
                        { gray = generateFullScale model.workingColor
                        , primary = getExistingShades "primary" model.scales
                        , danger = getExistingShades "danger" model.scales
                        , warning = getExistingShades "warning" model.scales
                        , success = getExistingShades "success" model.scales
                        }

                    else
                        { gray = getExistingShades "gray" model.scales
                        , primary = getExistingShades "primary" model.scales
                        , danger = getExistingShades "danger" model.scales
                        , warning = getExistingShades "warning" model.scales
                        , success = getExistingShades "success" model.scales
                        }

                _ ->
                    { gray = getExistingShades "gray" model.scales
                    , primary = getExistingShades "primary" model.scales
                    , danger = getExistingShades "danger" model.scales
                    , warning = getExistingShades "warning" model.scales
                    , success = getExistingShades "success" model.scales
                    }

        getColor shades level fallback =
            List.filter (\( l, _ ) -> l == level) shades
                |> List.head
                |> Maybe.map (Tuple.second >> oklchToHex)
                |> Maybe.withDefault fallback
    in
    { primary100 = getColor scales.primary 100 "#dbeafe"
    , primary200 = getColor scales.primary 200 "#bfdbfe"
    , primary300 = getColor scales.primary 300 "#93c5fd"
    , primary400 = getColor scales.primary 400 "#60a5fa"
    , primary500 = getColor scales.primary 500 "#3b82f6"
    , primary600 = getColor scales.primary 600 "#2563eb"
    , primary700 = getColor scales.primary 700 "#1d4ed8"
    , primary800 = getColor scales.primary 800 "#1e40af"
    , primary900 = getColor scales.primary 900 "#1e3a8a"
    , gray0 = getColor scales.gray 0 "#ffffff"
    , gray50 = getColor scales.gray 50 "#f9fafb"
    , gray100 = getColor scales.gray 100 "#f3f4f6"
    , gray200 = getColor scales.gray 200 "#e5e7eb"
    , gray300 = getColor scales.gray 300 "#d1d5db"
    , gray400 = getColor scales.gray 400 "#9ca3af"
    , gray500 = getColor scales.gray 500 "#6b7280"
    , gray600 = getColor scales.gray 600 "#4b5563"
    , gray700 = getColor scales.gray 700 "#374151"
    , gray800 = getColor scales.gray 800 "#1f2937"
    , gray900 = getColor scales.gray 900 "#111827"
    , gray950 = getColor scales.gray 950 "#030712"
    , danger100 = getColor scales.danger 100 "#fee2e2"
    , danger200 = getColor scales.danger 200 "#fecaca"
    , danger500 = getColor scales.danger 500 "#ef4444"
    , danger700 = getColor scales.danger 700 "#b91c1c"
    , danger800 = getColor scales.danger 800 "#991b1b"
    , danger900 = getColor scales.danger 900 "#7f1d1d"
    , warning100 = getColor scales.warning 100 "#fef3c7"
    , warning200 = getColor scales.warning 200 "#fde68a"
    , warning700 = getColor scales.warning 700 "#b45309"
    , warning800 = getColor scales.warning 800 "#92400e"
    , warning900 = getColor scales.warning 900 "#78350f"
    , success100 = getColor scales.success 100 "#dcfce7"
    , success200 = getColor scales.success 200 "#bbf7d0"
    , success500 = getColor scales.success 500 "#22c55e"
    , success700 = getColor scales.success 700 "#15803d"
    , success800 = getColor scales.success 800 "#166534"
    , success900 = getColor scales.success 900 "#14532d"
    }


getScaleShades : String -> List ColorScale -> List ( Int, Oklch )
getScaleShades name scales =
    List.filter (\s -> s.name == name) scales
        |> List.head
        |> Maybe.map .shades
        |> Maybe.withDefault []


getExistingShades : String -> List ColorScale -> List ( Int, Oklch )
getExistingShades name scales =
    getScaleShades name scales


viewExport : Model -> Html Msg
viewExport model =
    let
        colors =
            getPreviewColors model
    in
    div [ class "export-section" ]
        [ h2 [] [ text "CSS Export" ]
        , pre [] [ code [] [ text (exportSemanticCSS model.useSnakeCase colors) ] ]
        ]


exportSemanticCSS : Bool -> PreviewColors -> String
exportSemanticCSS useSnakeCase colors =
    let
        n name =
            if useSnakeCase then
                String.replace "-" "_" name
            else
                name

        ld light dark =
            "light-dark(" ++ light ++ ", " ++ dark ++ ")"
    in
    ":root {\n  color-scheme: light dark;\n\n  /* Surfaces */\n  --"
        ++ n "surface"
        ++ ": "
        ++ ld colors.gray0 colors.gray900
        ++ ";\n  --"
        ++ n "surface-secondary"
        ++ ": "
        ++ ld colors.gray50 colors.gray800
        ++ ";\n  --"
        ++ n "surface-elevated"
        ++ ": "
        ++ ld colors.gray0 colors.gray800
        ++ ";\n  --"
        ++ n "surface-sidebar"
        ++ ": "
        ++ colors.primary800
        ++ ";\n  --"
        ++ n "surface-sidebar-active"
        ++ ": "
        ++ colors.primary900
        ++ ";\n\n  /* Text */\n  --"
        ++ n "text"
        ++ ": "
        ++ ld colors.gray900 colors.gray100
        ++ ";\n  --"
        ++ n "text-secondary"
        ++ ": "
        ++ ld colors.gray700 colors.gray300
        ++ ";\n  --"
        ++ n "text-muted"
        ++ ": "
        ++ ld colors.gray400 colors.gray500
        ++ ";\n  --"
        ++ n "text-on-primary"
        ++ ": "
        ++ colors.gray0
        ++ ";\n  --"
        ++ n "text-sidebar"
        ++ ": "
        ++ colors.primary100
        ++ ";\n  --"
        ++ n "text-sidebar-muted"
        ++ ": "
        ++ colors.primary300
        ++ ";\n\n  /* Borders */\n  --"
        ++ n "border"
        ++ ": "
        ++ ld colors.gray200 colors.gray700
        ++ ";\n  --"
        ++ n "border-subtle"
        ++ ": "
        ++ ld colors.gray100 colors.gray800
        ++ ";\n\n  /* Interactive */\n  --"
        ++ n "link"
        ++ ": "
        ++ ld colors.primary500 colors.primary400
        ++ ";\n  --"
        ++ n "primary"
        ++ ": "
        ++ colors.primary500
        ++ ";\n  --"
        ++ n "primary-hover"
        ++ ": "
        ++ colors.primary600
        ++ ";\n\n  /* Status - Warning */\n  --"
        ++ n "warning-surface"
        ++ ": "
        ++ ld colors.warning100 colors.warning900
        ++ ";\n  --"
        ++ n "warning-border"
        ++ ": "
        ++ ld colors.warning200 colors.warning800
        ++ ";\n  --"
        ++ n "warning-text"
        ++ ": "
        ++ ld colors.warning700 colors.warning200
        ++ ";\n\n  /* Status - Danger */\n  --"
        ++ n "danger"
        ++ ": "
        ++ colors.danger500
        ++ ";\n  --"
        ++ n "danger-surface"
        ++ ": "
        ++ ld colors.danger100 colors.danger900
        ++ ";\n  --"
        ++ n "danger-border"
        ++ ": "
        ++ ld colors.danger200 colors.danger800
        ++ ";\n  --"
        ++ n "danger-text"
        ++ ": "
        ++ ld colors.danger700 colors.danger200
        ++ ";\n\n  /* Status - Success */\n  --"
        ++ n "success"
        ++ ": "
        ++ colors.success500
        ++ ";\n  --"
        ++ n "success-surface"
        ++ ": "
        ++ ld colors.success100 colors.success900
        ++ ";\n  --"
        ++ n "success-border"
        ++ ": "
        ++ ld colors.success200 colors.success800
        ++ ";\n  --"
        ++ n "success-text"
        ++ ": "
        ++ ld colors.success700 colors.success200
        ++ ";\n}\n\n/* Semantic utility classes */\n."
        ++ n "bg-surface"
        ++ " { background-color: var(--"
        ++ n "surface"
        ++ "); }\n."
        ++ n "bg-surface-secondary"
        ++ " { background-color: var(--"
        ++ n "surface-secondary"
        ++ "); }\n."
        ++ n "bg-surface-elevated"
        ++ " { background-color: var(--"
        ++ n "surface-elevated"
        ++ "); }\n."
        ++ n "bg-sidebar"
        ++ " { background-color: var(--"
        ++ n "surface-sidebar"
        ++ "); }\n."
        ++ n "bg-sidebar-active"
        ++ " { background-color: var(--"
        ++ n "surface-sidebar-active"
        ++ "); }\n\n."
        ++ n "text-primary"
        ++ " { color: var(--"
        ++ n "text"
        ++ "); }\n."
        ++ n "text-secondary"
        ++ " { color: var(--"
        ++ n "text-secondary"
        ++ "); }\n."
        ++ n "text-muted"
        ++ " { color: var(--"
        ++ n "text-muted"
        ++ "); }\n."
        ++ n "text-on-primary"
        ++ " { color: var(--"
        ++ n "text-on-primary"
        ++ "); }\n."
        ++ n "text-sidebar"
        ++ " { color: var(--"
        ++ n "text-sidebar"
        ++ "); }\n."
        ++ n "text-sidebar-muted"
        ++ " { color: var(--"
        ++ n "text-sidebar-muted"
        ++ "); }\n\n."
        ++ n "border-default"
        ++ " { border-color: var(--"
        ++ n "border"
        ++ "); }\n."
        ++ n "border-subtle"
        ++ " { border-color: var(--"
        ++ n "border-subtle"
        ++ "); }\n\n."
        ++ n "text-link"
        ++ " { color: var(--"
        ++ n "link"
        ++ "); }\n."
        ++ n "bg-primary"
        ++ " { background-color: var(--"
        ++ n "primary"
        ++ "); }\n."
        ++ n "bg-primary-hover"
        ++ ":hover { background-color: var(--"
        ++ n "primary-hover"
        ++ "); }\n\n."
        ++ n "bg-warning"
        ++ " { background-color: var(--"
        ++ n "warning-surface"
        ++ "); }\n."
        ++ n "border-warning"
        ++ " { border-color: var(--"
        ++ n "warning-border"
        ++ "); }\n."
        ++ n "text-warning"
        ++ " { color: var(--"
        ++ n "warning-text"
        ++ "); }\n\n."
        ++ n "bg-danger"
        ++ " { background-color: var(--"
        ++ n "danger-surface"
        ++ "); }\n."
        ++ n "border-danger"
        ++ " { border-color: var(--"
        ++ n "danger-border"
        ++ "); }\n."
        ++ n "text-danger"
        ++ " { color: var(--"
        ++ n "danger-text"
        ++ "); }\n\n."
        ++ n "bg-success"
        ++ " { background-color: var(--"
        ++ n "success-surface"
        ++ "); }\n."
        ++ n "border-success"
        ++ " { border-color: var(--"
        ++ n "success-border"
        ++ "); }\n."
        ++ n "text-success"
        ++ " { color: var(--"
        ++ n "success-text"
        ++ "); }"


-- COLOR CONVERSION (Oklch -> sRGB -> Hex)


oklchToHex : Oklch -> String
oklchToHex oklch =
    let
        -- Oklch to Oklab
        hRad =
            oklch.hue * pi / 180

        a =
            oklch.chroma * cos hRad

        b =
            oklch.chroma * sin hRad

        l =
            oklch.lightness

        -- Oklab to linear sRGB
        l_ =
            l + 0.3963377774 * a + 0.2158037573 * b

        m_ =
            l - 0.1055613458 * a - 0.0638541728 * b

        s_ =
            l - 0.0894841775 * a - 1.291485548 * b

        lCubed =
            l_ * l_ * l_

        mCubed =
            m_ * m_ * m_

        sCubed =
            s_ * s_ * s_

        rLinear =
            4.0767416621 * lCubed - 3.3077115913 * mCubed + 0.2309699292 * sCubed

        gLinear =
            -1.2684380046 * lCubed + 2.6097574011 * mCubed - 0.3413193965 * sCubed

        bLinear =
            -0.0041960863 * lCubed - 0.7034186147 * mCubed + 1.707614701 * sCubed

        -- Linear sRGB to sRGB (gamma correction)
        gammaCorrect x =
            if x <= 0.0031308 then
                12.92 * x

            else
                1.055 * (x ^ (1 / 2.4)) - 0.055

        -- Clamp to 0-1 range
        clamp x =
            Basics.max 0 (Basics.min 1 x)

        r =
            clamp (gammaCorrect rLinear)

        g =
            clamp (gammaCorrect gLinear)

        bVal =
            clamp (gammaCorrect bLinear)

        toHexPart f =
            let
                i =
                    round (f * 255)

                hex =
                    intToHex i
            in
            if String.length hex == 1 then
                "0" ++ hex

            else
                hex
    in
    "#" ++ toHexPart r ++ toHexPart g ++ toHexPart bVal


intToHex : Int -> String
intToHex n =
    let
        hexChars =
            "0123456789abcdef"

        clamped =
            Basics.max 0 (Basics.min 255 n)

        high =
            clamped // 16

        low =
            modBy 16 clamped
    in
    String.slice high (high + 1) hexChars ++ String.slice low (low + 1) hexChars
