module ViewHelpers exposing (..)

import Array
import Types exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Markdown


stackViewHelp : Int -> String -> Html Types.Msg
stackViewHelp i l =
    div [ class "stackitem" ]
        [ span [ class "stackindex" ] [ i |> toString |> text ]
        , div [ class "stackdata" ] [ text l ]
        ]


stackView : List String -> Html Types.Msg
stackView data =
    let
        newlist =
            if List.length data < 10 then
                List.append data (Array.initialize (10 - List.length data) (always "") |> Array.toList)
            else
                data
    in
        div [ class "row" ]
            [ div [ class "stackview col-sm-12 col-md-10 col-md-offset-1" ]
                (List.reverse
                    (List.indexedMap
                        stackViewHelp
                        newlist
                    )
                )
            ]


introView : Html Types.Msg
introView =
    div []
        [ div [ class "jumbotron" ]
            [ div [ class "container" ] [ h1 [] [ text "Abstract Data Structures: Stacks" ], p [] [ text """

Stacks are great! They allow us to store data in a way where the last data item we add is the first
item which is returned.

        """ ] ] ]
        ]


footer =
    div [ class "footer" ]
        [ text "All code for this site is written in Elm. "
        , a [ class "grey-link", href "mailto:matthew@mhoare.co.uk" ] [ text "Email me if you have any questions" ]
        , text "! — © 2016 Matthew Hoare"
        ]


stackOperationsPara =
    """
## Stack operations

There are only two legal operations which can be carried out on a stack.
1. **Push**
1. **Pop**

What does it mean to push and pop? Well, keep reading and you'll find out.

### Push
When we talk about *pushing* onto the stack, what we really mean is adding a new item onto the top of the stack. Once we have *pushed* items onto the stack we can access the top element. Use the [interactive stack](#interactivestack) at the top of the page to see what I mean.

### Pop
Okay, so we have worked out how to add items to the stack, but what happens when we want to access them?

When we want to access items on the stack, we can only access and remove the top element.
This is called *popping*, you will *pop* the top element off of the stack.

You don't understand? Don't worry, lets give and example.

You have pushed the calculation shown above onto the stack. The stack now looks like this:

See, the top element is `1 + 2` and the second is `+ 3` and so on.

Now we are going to *pop* `1 + 2` off of the stack and evaluate it. Now the stack looks like this:

Did you notice that `1 + 2` is no longer on the top of the stack? It is now `+ 3`. So we *pop* that from the stack as well and evaluate that operation applied to our previous value. Our total is now 6.

We will continue this process until the stack is empty.

You may be thinking, "Why do you remove the item when popping it off?" The reason we do that is so that we can access the next item. Remember we can only access the top item in the stack.
"""


secondPara =
    """
## Stacks... Why?

You may be thinking - why would we want only want to access the last item we added?

Well, think about what happens when you use brackets in this calculation:

```( ( ( 1 + 2 ) + 3 ) + 4 )```

You first evalute the deepest brackets and then use that value when evaluating the brackets in which it was nested in and so on. Here you need to know calculate a value and then use that value in the next calculation.

**This is exactly where a stack would be used in computing.** Each operation would be pushed onto the stack, meaning that the `( 1 + 2 )` operation would be on the top of the stack. Then each item is popped off of the stack, evaluated and then the next item is popped off.

Stacks are also used when *calling procedures in programs* and *when reversing array items*.
"""


substituteNothing l =
    case l of
        Just l ->
            l

        Nothing ->
            ""
