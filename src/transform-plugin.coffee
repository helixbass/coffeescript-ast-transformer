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
  constructor: (@parent) ->
    @declaredVariables = []

  addDeclaredVariable: (name) ->
    @declaredVariables.push name

  hasDeclaredVariables: ->
    !!@declaredVariables.length

  freeVariable: (name, {single, reserve = yes} = {}) ->
    index = 0
    loop
      temp = Scope.getTemporary name, index, single
      break unless @check(temp) # or temp in @root.referencedVars
      index++
    @addDeclaredVariable temp if reserve
    temp

  @getTemporary: (name, index, single = no) ->
    if single
      startCode = name.charCodeAt 0
      endCode = 'z'.charCodeAt 0
      diff = endCode - startCode
      newCode = startCode + index % (diff + 1)
      letter = String.fromCharCode newCode
      num = index // (diff + 1)
      "#{letter}#{num or ''}"
    else
      "#{name}#{index or ''}"

  check: (name) ->
    !! @type name

  type: (name) ->
    for declaredVariable in @declaredVariables when declaredVariable.name is name
      return 'var'
    null

transformer = ({types: t}) ->
  addVariableDeclarations = (path, scope) ->
    path.unshiftContainer 'body',
      t.variableDeclaration 'var', scope.declaredVariables.map (name) ->
        t.variableDeclarator t.identifier name

  isFunction = (node) ->
    node.type in ['FunctionExpression', 'ArrowFunctionExpression']

  makeReturn = (path, {replacePath = path, resultsVariableName} = {}) ->
    {node} = path

    switch node.type
      when 'BlockStatement'
        {body} = node
        return unless body.length
        makeReturn path.get("body.#{body.length - 1}"), {resultsVariableName}
      when 'ExpressionStatement'
        makeReturn path.get('expression'), {replacePath: path, resultsVariableName}
      when 'ReturnStatement'
        null
      when 'For'
        node.returns = yes
      else
        replacePath.replaceWith(
          if resultsVariableName?
            t.callExpression(
              t.memberExpression(t.identifier(resultsVariableName), t.identifier('push')),
              [node]
            )
          else
            t.returnStatement node
        )

  blockWrap = (nodes) ->
    return nodes[0] if nodes.length is 1 and t.isBlockStatement nodes[0]
    t.blockStatement nodes

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
    'FunctionExpression|ArrowFunctionExpression': (path) ->
      makeReturn path.get 'body'
    For: (path, {scope}) ->
      {node: {body: bodyOriginal, source, name, returns}, node} = path

      node.body = body = blockWrap [bodyOriginal]

      indexVariableName = scope.freeVariable 'i', single: yes
      indexVariableIdentifier = t.identifier indexVariableName
      indexInitialization = t.assignmentExpression(
        '='
        indexVariableIdentifier
        t.numericLiteral 0
      )
      lengthVariableName = scope.freeVariable 'len'
      lengthVariableIdentifier = t.identifier lengthVariableName
      lengthInitialization = t.assignmentExpression(
        '='
        lengthVariableIdentifier
        t.memberExpression source, t.identifier 'length'
      )
      init = t.sequenceExpression [
        indexInitialization
        lengthInitialization
      ]
      test = t.binaryExpression '<', indexVariableIdentifier, lengthVariableIdentifier
      update = t.updateExpression '++', indexVariableIdentifier, no
      body.body.unshift(
        t.expressionStatement(
          t.assignmentExpression(
            '='
            name
            t.memberExpression source, indexVariableIdentifier, yes
          )
        )
      )

      if returns
        returnsVariableName = scope.freeVariable 'results'
        returnsVariableIdentifier = t.identifier returnsVariableName
        makeReturn path.get('body'), resultsVariableName: returnsVariableName

      forStatement = t.forStatement(
        init
        test
        update
        body
      )
      if returns
        path.replaceWithMultiple [
          t.expressionStatement t.assignmentExpression(
            '='
            returnsVariableIdentifier
            t.arrayExpression []
          )
          forStatement
          t.returnStatement returnsVariableIdentifier
        ]
      else
        path.replaceWith forStatement
  )

module.exports = transformer
