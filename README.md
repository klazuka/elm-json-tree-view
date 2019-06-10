# Elm JSON Tree View

This library provides a JSON tree view. You feed it JSON, and it transforms it into interactive HTML.

Try the [online demo](https://klazuka.github.io/elm-json-tree-view/example/index.html) ([source](https://github.com/klazuka/elm-json-tree-view/blob/master/example/src/Main.elm))

Features:

  - show JSON as a tree of HTML
  - expand/collapse nodes in the tree
  - expand/collapse the entire tree
  - select scalar values in the tree

## History

I originally published this as [Microsoft/elm-json-tree-view](https://github.com/microsoft/elm-json-tree-view), but in May 2019 Microsoft changed their GitHub organization name which broke `elm package`. Rather than waiting for a fix in the Elm package repository, I have decided to move all work over to my personal GitHub account and re-publish.

## Usage

See the [docs](http://package.elm-lang.org/packages/klazuka/elm-json-tree-view/latest) or look at the example app's [source code](https://github.com/klazuka/elm-json-tree-view/blob/master/example/src/Main.elm).

But if you really insist on something super simple, here goes:
```elm
import JsonTree
import Html exposing (text)

main =
    JsonTree.parseString """[1,2,3]"""
        |> Result.map (\tree -> JsonTree.view tree config JsonTree.defaultState)
        |> Result.withDefault (text "Failed to parse JSON")

config = { onSelect = Nothing, toMsg = always (), colors = JsonTree.defaultColors }
```

Note that the above example is only meant to give you a taste. It does not wire everything up, which means that some things will be broken (i.e. expand/collapse). See the [docs](http://package.elm-lang.org/packages/klazuka/elm-json-tree-view/latest) and the example app for more details. 

## Thanks

UI based on Gildas Lormeau's [JSONView](https://github.com/gildas-lormeau/JSONView-for-Chrome) Chrome extension.