babel = require '@babel/core'
{default: generate} = require '@babel/generator'

transformCoffeePlugin = require './transform-plugin'

getTransformedAst = (astCoffee, {print} = {}) ->
  {ast: astTransformed} = babel.transformFromAstSync astCoffee, null,
    code: no
    ast: yes
    plugins: [transformCoffeePlugin]
  return astTransformed unless print
  {code: printed} = generate(astTransformed)
  printed

module.exports = {getTransformedAst}
