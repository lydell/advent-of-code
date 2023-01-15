# Elm template for Advent of Code

1. Clone this repo.
2. `npm ci`
3. `cp src/TemplateSimple.elm src/Day1.elm`
4. `npx elm-live src/Day1.elm -- --output=elm.js`
5. Code away and see the results in the browser!

- TemplateSimple.elm: `main` is just an `Html` element.
- TemplateSandbox.elm: If you need `Browser.sandbox`.
- TemplateElement.elm: If you need `Browser.element`.

In case tests help you solve a tricky puzzle, elm-test is set up: `npx elm-test --watch`.
