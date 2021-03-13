{mapValues} = require 'lodash/fp'
{default: template} = require '@babel/template'

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
    @positions = {}
    @utilities = {} unless @parent
    @root = @parent?.root ? @

  addDeclaredVariable: (name, type) ->
    if Object::hasOwnProperty.call @positions, name
      @declaredVariables[@positions[name]].type = type
    else
      @positions[name] = @declaredVariables.push({name, type}) - 1

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

  assign: (name, value) ->
    @addDeclaredVariable name, {value, assigned: yes}
    @hasAssignments = yes

transformer = ({types: t}) ->
  addVariableDeclarations = (path, scope) ->
    path.unshiftContainer 'body',
      t.variableDeclaration 'var', scope.declaredVariables.map ({name, type}) ->
        t.variableDeclarator t.identifier(name), if type?.assigned then type.value

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

  getIsAssignable = (node) ->
    switch node.type
      when 'ArrayPattern'
        {elements} = node
        for element, index in elements
          return no if element.type is 'RestElement' and index < elements.length - 1
          return no if element.type is 'RestElement' and not element.argument?
        yes
      else
        yes

  UTILITIES =
    slice: ->
      template.expression.ast '[].slice'
    splice: ->
      template.expression.ast '[].splice'
  utility = (name, scope) ->
    {root} = scope
    if name of root.utilities
      root.utilities[name]
    else
      ref = root.freeVariable name
      root.assign ref, UTILITIES[name]()
      root.utilities[name] = ref

  nodesToSkip = new WeakSet()

  handleDestructuringAssignment = (path, scope) ->
    {node: {left: {elements}, right}} = path

    slicer = (type) -> (object, startIndex) ->
      args = [object, t.numericLiteral(startIndex)]
      t.callExpression(
        template.expression.ast "#{utility(type, scope)}.call"
        args
      )
    generateSlice = slicer 'slice'
    generateSplice = slicer 'splice'

    splatOrExpansionIndex = elements.findIndex (element) -> element?.type is 'RestElement'
    isSplat = elements[splatOrExpansionIndex].argument?
    leftElements = elements.slice 0, splatOrExpansionIndex + (if isSplat then 1 else 0)
    rightElements = elements.slice splatOrExpansionIndex + 1
    rhsReference = right
    assignments = []
    pushAssignment = (lhs, rhs) ->
      assignments.push t.assignmentExpression '=', lhs, rhs
    if leftElements.length
      pushAssignment t.arrayPattern(leftElements), rhsReference
    if rightElements.length
      rightElementsAssignmentRhs =
        if isSplat
          generateSplice( )
        else
          generateSlice rhsReference, rightElements.length * -1
      pushAssignment t.arrayPattern(rightElements), rightElementsAssignmentRhs
    path.parentPath.replaceWithMultiple assignments.map (assignment) ->
      expressionStatement = t.expressionStatement assignment
      nodesToSkip.add expressionStatement
      expressionStatement

  visitor: withNullReturnValues(
    Program:
      enter: (_, state) ->
        state.scope = new Scope
      exit: (path, {scope}) ->
        addVariableDeclarations(path, scope) if scope.hasDeclaredVariables()
    Identifier: ({node: {declaration, name}}, {scope}) ->
      scope.addDeclaredVariable name if declaration
    'BinaryExpression|LogicalExpression': (path) ->
      {node: {operator}, node} = path

      CONVERSIONS =
        is: '==='
        and: '&&'
        or: '||'

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

    ExpressionStatement: (path) ->
      {node} = path
      path.skip() if nodesToSkip.has node

    AssignmentExpression: (path, {scope}) ->
      {node: {left}} = path

      if left.type is 'ArrayPattern' and not getIsAssignable(left)
        handleDestructuringAssignment(path, scope)
  )

module.exports = transformer
