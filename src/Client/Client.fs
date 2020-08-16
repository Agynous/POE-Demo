module Client

open Elmish
open Elmish.React
open Fable.React
open Fable.React.Props
open Fetch.Types
open Thoth.Fetch
open Thoth.Json

open Shared

// The model holds data that you want to keep track of while the application is running
// in this case, we are keeping track of a counter
// we mark it as optional, because initially it will not be available from the client
// the initial value will be requested from server
type Model = { Counter: Counter option }

// The Msg type defines what events/actions can occur while the application is running
// the state of the application changes *only* in reaction to these events
type Msg =
    | Increment
    | Decrement
    | InitialCountLoaded of Counter

let initialCounter () = Fetch.fetchAs<unit, Counter> "/api/init"

// defines the initial state and initial command (= side-effect) of the application
let init () : Model * Cmd<Msg> =
    let initialModel = { Counter = None }
    let loadCountCmd =
        Cmd.OfPromise.perform initialCounter () InitialCountLoaded
    initialModel, loadCountCmd

// The update function computes the next state of the application based on the current state and the incoming events/messages
// It can also run side-effects (encoded as commands) like calling the server via Http.
// these commands in turn, can dispatch messages to which the update function will react.
let update (msg : Msg) (currentModel : Model) : Model * Cmd<Msg> =
    match currentModel.Counter, msg with
    | Some counter, Increment ->
        let nextModel = { currentModel with Counter = Some { Value = counter.Value + 1 } }
        nextModel, Cmd.none
    | Some counter, Decrement ->
        let nextModel = { currentModel with Counter = Some { Value = counter.Value - 1 } }
        nextModel, Cmd.none
    | _, InitialCountLoaded initialCount->
        let nextModel = { Counter = Some initialCount }
        nextModel, Cmd.none
    | _ -> currentModel, Cmd.none


let safeComponents =
    let components =
        span [ ]
           [ a [ Href "https://github.com/SAFE-Stack/SAFE-template" ]
               [ str "SAFE  "
                 str Version.template ]
             str ", "
             a [ Href "https://github.com/giraffe-fsharp/Giraffe" ] [ str "Giraffe" ]
             str ", "
             a [ Href "http://fable.io" ] [ str "Fable" ]
             str ", "
             a [ Href "https://elmish.github.io" ] [ str "Elmish" ]

           ]

    span [ ]
        [ str "Version "
          strong [ ] [ str Version.app ]
          str " powered by: "
          components ]

let show = function
    | { Counter = Some counter } -> string counter.Value
    | { Counter = None   } -> "Loading..."

let view (model : Model) (dispatch : Msg -> unit) =
    div[] [
     nav [ Class "navbar" ]
        [ div [ Class "container" ]
            [ div [ Class "navbar-brand" ]
                [ a [ Class "navbar-item"
                      Href "#" ]
                    [ img [ Src "https://cdn.emk.dev/templates/bulma-logo-light.png"
                            Alt "Logo" ] ]
                  span [ Class "navbar-burger burger"
                         Data ("target", "navbarMenu") ]
                    [ span [ ]
                        [ ]
                      span [ ]
                        [ ]
                      span [ ]
                        [ ] ] ]
              div [ Id "navbarMenu"
                    Class "navbar-menu" ]
                [ div [ Class "navbar-end" ]
                    [ a [ Class "navbar-item is-active" ]
                        [ str "Home" ]
                      a [ Class "navbar-item" ]
                        [ str "Examples" ]
                      a [ Class "navbar-item" ]
                        [ str "Features" ]
                      a [ Class "navbar-item" ]
                        [ str "Team" ]
                      a [ Class "navbar-item" ]
                        [ str "Archives" ]
                      a [ Class "navbar-item" ]
                        [ str "Help" ] ] ] ] ]
     div [ Class "container" ]
        [ div [ Class "section" ]
            [ div [ Class "columns" ]
                [ div [ Class "column has-text-centered" ]
                    [ h1 [ Class "title"
                           Style [ Color "ghostwhite" ] ]
                        [ str "Bulma Card Layout Template" ]
                      br [ ] ] ]
              div [ Id "app"
                    Class "row columns is-multiline" ]
                [ div [ HTMLAttr.Custom ("v-for", "card in cardData")
                        HTMLAttr.Custom ("key", "card.id")
                        Class "column is-4" ]
                    [ div [ Class "card large" ]
                        [ div [ Class "card-image is-16by9" ]
                            [ figure [ Class "image" ]
                                [ img [ Alt "Image" ] ] ]
                          div [ Class "card-content" ]
                            [ div [ Class "media" ]
                                [ div [ Class "media-left" ]
                                    [ figure [ Class "image is-48x48" ]
                                        [ img [ Alt "Image" ] ] ]
                                  div [ Class "media-content" ]
                                    [ p [ Class "title is-4 no-padding" ]
                                        [ str "{{card.user.title}}" ]
                                      p [ ]
                                        [ span [ Class "title is-6" ]
                                            [ a [  ]
                                                [ str "{{card.user.handle}}" ] ] ]
                                      p [ Class "subtitle is-6" ]
                                        [ str "{{card.user.title}}" ] ] ]
                              div [ Class "content" ]
                                [ str "{{card.content}}"
                                  div [ Class "background-icon" ]
                                    [ span [ Class "icon-twitter" ]
                                        [ ] ] ] ] ] ] ] ] ]
     footer [ Class "footer" ]
        [ div [ Class "container" ]
            [ div [ Class "content has-text-centered" ]
                [ div [ Class "soc" ]
                    [ a [ Href "#" ]
                        [ i [ Class "fa fa-github-alt fa-lg"
                              HTMLAttr.Custom ("aria-hidden", "true") ]
                            [ ] ]
                      a [ Href "#" ]
                        [ i [ Class "fa fa-youtube fa-lg"
                              HTMLAttr.Custom ("aria-hidden", "true") ]
                            [ ] ]
                      a [ Href "#" ]
                        [ i [ Class "fa fa-facebook fa-lg"
                              HTMLAttr.Custom ("aria-hidden", "true") ]
                            [ ] ]
                      a [ Href "#" ]
                        [ i [ Class "fa fa-twitter fa-lg"
                              HTMLAttr.Custom ("aria-hidden", "true") ]
                            [ ] ] ]
                  p [ ]
                    [ strong [ ]
                        [ str "Bulma" ]
                      str "by"
                      a [ Href "http://jgthms.com" ]
                        [ str "Jeremy Thomas" ]
                      str ".
                The source code is licensed"
                      a [ Href "http://opensource.org/licenses/mit-license.php" ]
                        [ str "MIT" ]
                      str "."
                      br [ ] ] ] ] ] ]

#if DEBUG
open Elmish.Debug
open Elmish.HMR
#endif

Program.mkProgram init update view
#if DEBUG
|> Program.withConsoleTrace
#endif
|> Program.withReactBatched "elmish-app"
#if DEBUG
|> Program.withDebugger
#endif
|> Program.run
