import Browser
import Html
import Html.Attributes as HTMLATTR
import Html.Events as HEv
import Http
import Json.Decode as JSD
import Time
import Json.Decode.Pipeline as JSP
-- import Json


main = Browser.element {init = init, update = update, view = view, subscriptions = subscr}

-- MODEL

type alias Flags = { endpoint : String }

type alias Point = { lat : Float, lng : Float }

type alias Location =
  { name : String
  , pressure : Float
  , imgurl : String
  , location : Point
  , capacity : Int
  , time : Time.Posix
  , visitors : Int
  , infourl : Maybe String
  , description : Maybe String
  }

type LocData = Loading | Failed String | Success (List Location)

jsonDataRequest endpoint = Http.get
      { url = endpoint
      , expect = Http.expectJson ReceivedLocations locationListDecoder
      }

type alias Model =
  { sort : List Location -> List Location
  , locations : LocData
  , endpoint : String
  }

init : Flags -> (Model, Cmd Msg)
init { endpoint } =
  ( { sort = List.sortBy .name, locations = Loading, endpoint = endpoint }
  , jsonDataRequest endpoint
  )

-- UPDATE
type Msg = ReceivedLocations (Result Http.Error (List Location))
         | Tick Time.Posix
         | SortByChanged String
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    ReceivedLocations (Ok locations) ->
          ({model | locations = Success locations }, Cmd.none)
    ReceivedLocations (Err err) ->
          ({model | locations = Failed ("An error occurred while loading the json: " ++ Debug.toString err) }, Cmd.none)
    Tick time ->
      (model, jsonDataRequest model.endpoint)
    SortByChanged value ->
      ({model | sort = sortFromValue value}, Cmd.none)


-- SUBSCRIPTIONS
subscr: Model -> Sub Msg
subscr mdl =
  Time.every 5000 Tick


-- VIEW

locationToHtml : Location -> Html.Html Msg
locationToHtml loc =
  let class name = HTMLATTR.class name
      attribute name = HTMLATTR.attribute name
  in

    Html.div [class "loc-row"] [
      -- the image
      Html.div [class "loc-img"]
               [Html.img [attribute "src" loc.imgurl] []],
      -- the rest
      Html.div []
        [
          -- upper
          Html.div [class "loc-upper"] [
                     -- Name
                     Html.div [class "loc-name"] [Html.text loc.name],
                     -- Progress bar
                     Html.div [class "loc-progress"] []
                   ],
          -- lower
          Html.div [class "loc-lower"] [
                     -- info icon
                     case loc.infourl of
                       Just url ->
                         Html.a [attribute "href" url] [
                           Html.img [attribute "src" "img/info.svg", class "loc-info"] [] 
                         ]
                       Nothing ->
                         Html.div[attribute "src" "img/info.svg", class "loc-info"] []
                        , 
                     -- Text
                     Html.div [class "loc-description"] [Html.text (Maybe.withDefault "" loc.description)],
                     -- Right side
                     Html.div [class "loc-right"] [
                        Html.div [class "loc-location"
                        ] [
                          Html.img [attribute "src" "img/map-marker.svg"] [],
                          Html.text "542m"
                        ],
                        Html.button [class "loc-route"] [
                          Html.text "route"
                        ]
                     ]
                   ]
        ]
    ]

sortFromValue : String -> List Location -> List Location
sortFromValue str = case str of
    "pressure" -> List.sortBy .pressure
    _ ->  List.sortBy .name

locationHeaders : Html.Html Msg
locationHeaders =
  let divClass (class, child) = Html.div [HTMLATTR.class class] [Html.text child]
  in
    Html.div [HTMLATTR.class "loc-header-row"] (
      [ Html.select [HEv.onInput SortByChanged ]
        [ Html.option [HTMLATTR.value "alphabetically"] [Html.text "alphabetically"]
        , Html.option [HTMLATTR.value "pressure"] [Html.text "pressure"]
        ]
      ] ++
      List.map divClass [
        ("loc-header-cell-img", "Image"),
        ("loc-header-cell-name", "Name"),
        ("loc-header-cell-pressure", "Pressure")
      ]
    )

locationToString : Location -> String
locationToString loc =
  loc.name ++ " " ++ String.fromFloat loc.pressure

view : Model -> Html.Html Msg
view model =
  case model.locations of
    Loading ->
      Html.div [] [Html.text "Loading"]
    Failed msg ->
      Html.div [] [Html.text msg]
    Success data ->
      Html.div [HTMLATTR.class "locations"]
       (List.append [locationHeaders] (List.map locationToHtml (model.sort data)))

-- DECODERS

pointDecoder : JSD.Decoder Point
pointDecoder =
  let lat = JSD.field "latitude" JSD.float
      lng = JSD.field "longitude" JSD.float
  in JSD.map2 Point lat lng

posixDecoder : JSD.Decoder Time.Posix
posixDecoder = JSD.map ((*) 1000 >> truncate >> Time.millisToPosix) JSD.float

locationDecoder : JSD.Decoder Location
locationDecoder = JSD.succeed Location
                   |> JSP.required "name" JSD.string
                   |> JSP.required "pressure" JSD.float
                   |> JSP.required "image" JSD.string
                   |> JSP.custom pointDecoder
                   |> JSP.required "capacity" JSD.int
                   |> JSP.required "lastTimestamp" posixDecoder
                   |> JSP.required "visitors" JSD.int
                   |> JSP.optional "infourl" (JSD.nullable JSD.string) Nothing
                   |> JSP.optional "description" (JSD.nullable JSD.string) Nothing

locationListDecoder : JSD.Decoder (List Location)
locationListDecoder = JSD.field "locations" (JSD.list locationDecoder)

-- HELPERS

-- Given the clients location as Point an Location returns the distance in m
distance : Point -> Location -> Float
distance _ _ = 542
