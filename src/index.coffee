babel = require '@babel/core'

transformCoffeePlugin = require './transform-plugin'

getTransformedAst = (astCoffee) ->
  {ast} = babel.transformFromAstSync astCoffee, null,
    code: no
    ast: yes
    plugins: [transformCoffeePlugin]
  ast

module.exports = {getTransformedAst}
