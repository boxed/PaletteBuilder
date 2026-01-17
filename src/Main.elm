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
    }


shadeLevels : List Int
shadeLevels =
    [ 50, 100, 200, 300, 400, 500, 600, 700, 800, 900, 950 ]


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
            if level == 500 then
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
            [ div [ class "left-panel" ]
                [ viewScaleSelector model
                , viewWorkflow model
                ]
            , div [ class "right-panel" ]
                [ viewLivePreview model
                , viewChatPreview model
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
        (List.map (viewCompactScale activeLevel (model.step == Done)) previewScales)


viewCompactScale : Int -> Bool -> ( String, List ( Int, Oklch ) ) -> Html Msg
viewCompactScale activeLevel canEdit ( name, shades ) =
    div [ class "compact-scale" ]
        [ span [ class "compact-scale-name" ] [ text name ]
        , div [ class "compact-shades" ]
            (List.map (viewCompactShade name shades activeLevel canEdit) (List.map Tuple.first shades |> List.sort))
        ]


viewCompactShade : String -> List ( Int, Oklch ) -> Int -> Bool -> Int -> Html Msg
viewCompactShade scaleName shades activeLevel canEdit level =
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
            div
                [ class ("compact-shade" ++ activeClass isActive ++ (if canEdit then " clickable" else ""))
                , style "background-color" (oklchToHex oklch)
                , if canEdit then onClick (OpenShadeEditor scaleName level oklch) else class ""
                , title (String.fromInt level ++ ": " ++ oklchToHex oklch)
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
    in
    div [ class "chat-preview" ]
        [ div
            [ class "chat-sidebar"
            , style "background-color" colors.primary800
            ]
            [ div [ class "chat-workspace" ]
                [ span [ style "color" colors.white ] [ text "Workspace" ]
                , div [ class "chat-user" ]
                    [ span [ class "status-dot", style "background-color" colors.success500 ] []
                    , span [ style "color" colors.primary200 ] [ text "You" ]
                    ]
                ]
            , div [ class "chat-nav" ]
                [ div [ class "chat-nav-item", style "color" colors.primary100 ] [ text "Inbox" ]
                , div [ class "chat-nav-item", style "color" colors.primary100 ] [ text "Starred" ]
                ]
            , div [ class "chat-channels-label", style "color" colors.primary300 ] [ text "CHANNELS" ]
            , div [ class "chat-channels" ]
                [ div
                    [ class "chat-channel selected"
                    , style "background-color" colors.primary900
                    , style "color" colors.white
                    ]
                    [ text "# design" ]
                , div
                    [ class "chat-channel"
                    , style "color" colors.primary100
                    ]
                    [ text "# engineering"
                    , span
                        [ class "unread-badge"
                        , style "background-color" colors.danger500
                        , style "color" colors.white
                        ]
                        [ text "4" ]
                    ]
                , div [ class "chat-channel", style "color" colors.primary100 ] [ text "# marketing" ]
                ]
            ]
        , div [ class "chat-main", style "background-color" colors.white ]
            [ div [ class "chat-header", style "border-color" colors.gray200 ]
                [ span [ style "color" colors.gray900 ] [ text "# design" ] ]
            , div
                [ class "chat-alert"
                , style "background-color" colors.warning100
                , style "border-color" colors.warning200
                ]
                [ div [ class "alert-icon", style "color" colors.warning700 ] [ text "!" ]
                , div []
                    [ div [ style "color" colors.warning700 ] [ text "Oops!" ]
                    , div [ style "color" colors.warning700 ] [ text "Connection problem." ]
                    ]
                ]
            , div [ class "chat-messages" ]
                [ div [ class "chat-message" ]
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
                , div [ class "chat-message" ]
                    [ div [ class "message-avatar", style "background-color" colors.gray400 ] []
                    , div [ class "message-content" ]
                        [ div [ class "message-header" ]
                            [ span [ class "message-author", style "color" colors.gray900 ] [ text "Tiffany Myers" ]
                            , span [ class "message-time", style "color" colors.gray400 ] [ text "12:51 PM" ]
                            ]
                        , div [ class "message-text", style "color" colors.gray700 ]
                            [ span [ style "color" colors.primary500 ] [ text "@sarah " ]
                            , text "I put the photos in the shared folder."
                            ]
                        ]
                    ]
                ]
            , div [ class "chat-input", style "background-color" colors.gray50, style "border-color" colors.gray200 ]
                [ span [ style "color" colors.gray400 ] [ text "Type your message..." ] ]
            ]
        ]


type alias PreviewColors =
    { primary100 : String
    , primary200 : String
    , primary300 : String
    , primary400 : String
    , primary500 : String
    , primary700 : String
    , primary800 : String
    , primary900 : String
    , gray50 : String
    , gray100 : String
    , gray200 : String
    , gray300 : String
    , gray400 : String
    , gray500 : String
    , gray700 : String
    , gray900 : String
    , danger500 : String
    , warning100 : String
    , warning200 : String
    , warning700 : String
    , success500 : String
    , white : String
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
    , primary700 = getColor scales.primary 700 "#1d4ed8"
    , primary800 = getColor scales.primary 800 "#1e40af"
    , primary900 = getColor scales.primary 900 "#1e3a8a"
    , gray50 = getColor scales.gray 50 "#f9fafb"
    , gray100 = getColor scales.gray 100 "#f3f4f6"
    , gray200 = getColor scales.gray 200 "#e5e7eb"
    , gray300 = getColor scales.gray 300 "#d1d5db"
    , gray400 = getColor scales.gray 400 "#9ca3af"
    , gray500 = getColor scales.gray 500 "#6b7280"
    , gray700 = getColor scales.gray 700 "#374151"
    , gray900 = getColor scales.gray 900 "#111827"
    , danger500 = getColor scales.danger 500 "#ef4444"
    , warning100 = getColor scales.warning 100 "#fef3c7"
    , warning200 = getColor scales.warning 200 "#fde68a"
    , warning700 = getColor scales.warning 700 "#b45309"
    , success500 = getColor scales.success 500 "#22c55e"
    , white = "#ffffff"
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
        completedScales =
            List.filter (\s -> List.length s.shades == 11) model.scales
    in
    if List.isEmpty completedScales then
        text ""

    else
        div [ class "export-section" ]
            [ h2 [] [ text "Export" ]
            , div [ class "export-formats" ]
                [ div [ class "export-block" ]
                    [ h4 [] [ text "CSS Variables" ]
                    , pre [] [ code [] [ text (exportCSS completedScales) ] ]
                    ]
                , div [ class "export-block" ]
                    [ h4 [] [ text "Tailwind Config" ]
                    , pre [] [ code [] [ text (exportTailwind completedScales) ] ]
                    ]
                ]
            ]


exportCSS : List ColorScale -> String
exportCSS scales =
    let
        scaleToCSS scale =
            List.map
                (\( level, oklch ) ->
                    "  --" ++ scale.name ++ "-" ++ String.fromInt level ++ ": " ++ oklchToHex oklch ++ ";"
                )
                scale.shades
                |> String.join "\n"
    in
    ":root {\n" ++ (List.map scaleToCSS scales |> String.join "\n\n") ++ "\n}"


exportTailwind : List ColorScale -> String
exportTailwind scales =
    let
        scaleToTailwind scale =
            let
                shadeLines =
                    List.map
                        (\( level, oklch ) ->
                            "        " ++ String.fromInt level ++ ": '" ++ oklchToHex oklch ++ "',"
                        )
                        scale.shades
                        |> String.join "\n"
            in
            "      '" ++ scale.name ++ "': {\n" ++ shadeLines ++ "\n      },"
    in
    "module.exports = {\n  theme: {\n    extend: {\n      colors: {\n"
        ++ (List.map scaleToTailwind scales |> String.join "\n")
        ++ "\n      }\n    }\n  }\n}"



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
