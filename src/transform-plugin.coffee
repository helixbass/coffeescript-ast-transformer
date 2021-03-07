{mapValues} = require 'lodash/fp'

withNullReturnValues = mapValues (f) ->
  if f.enter or f.exit
    enter: (...args) ->
      f.enter? ...args
      null
    exit: (...args) ->
      f.exit? ...args
      null
  else
    (...args) ->
      f ...args
      null

class Scope
  constructor: ->
    @declaredVariables = []

  addDeclaredVariable: (name) ->
    @declaredVariables.push name

  hasDeclaredVariables: ->
    !!@declaredVariables.length

transformer = ({types: t}) ->
  addVariableDeclarations = (path, scope) ->
    path.unshiftContainer 'body',
      t.variableDeclaration 'var', scope.declaredVariables.map (name) ->
        t.variableDeclarator t.identifier name

  isFunction = (node) ->
    node.type in ['FunctionExpression', 'ArrowFunctionExpression']

  visitor: withNullReturnValues(
    Program:
      enter: (_, state) ->
        state.scope = new Scope
      exit: (path, {scope}) ->
        addVariableDeclarations(path, scope) if scope.hasDeclaredVariables()
    Identifier: ({node: {declaration, name}}, {scope}) ->
      scope.addDeclaredVariable name if declaration
    BinaryExpression: (path) ->
      {node: {operator}, node} = path

      CONVERSIONS =
        'is': '==='

      if operator of CONVERSIONS
        node.operator = CONVERSIONS[operator]

    UnaryExpression: (path) ->
      {node: {operator, argument}} = path

      if operator is 'do'
        func =
          if t.isAssignmentExpression(argument) and isFunction(argument.right)
            argument.right
          else
            argument

        path.replaceWith t.callExpression(
          argument,
          func.params?.map((param, paramIndex) ->
            if t.isAssignmentPattern param
              func.params[paramIndex] = param.left
              param.right
            else
              param
          ) ? []
        )
  )

module.exports = transformer
