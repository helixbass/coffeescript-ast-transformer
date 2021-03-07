babel = require '@babel/core'
{default: generate} = require '@babel/generator'

transformCoffeePlugin = require './transform-plugin'
overrideBabelTypes = require './override-babel-types'

getTransformedAst = (astCoffee, {print} = {}) ->
  overrideBabelTypes()
  {ast: astTransformed} = babel.transformFromAstSync astCoffee, null,
    code: no
    ast: yes
    plugins: [transformCoffeePlugin]
  return astTransformed unless print
  {code: printed} = generate(astTransformed)
  printed

module.exports = {getTransformedAst}
