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

  addDeclaredVariable: (name, type, immediate) ->
    return @parent.addDeclaredVariable name, type if @shared and not immediate
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
    @addDeclaredVariable temp, 'var', yes if reserve
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
    {node: {body}} = path

    bodyContainerSubpath =
      if Array.isArray body
        path
      else
        path.get 'body'

    bodyContainerSubpath.unshiftContainer 'body',
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
          continue unless element?
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
    splatOrExpansionElement = elements[splatOrExpansionIndex]
    isSplat = splatOrExpansionElement.argument?
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
          generateSplice splatOrExpansionElement.argument, rightElements.length * -1
        else
          generateSlice rhsReference, rightElements.length * -1
      pushAssignment t.arrayPattern(rightElements), rightElementsAssignmentRhs
    path.parentPath.replaceWithMultiple assignments.map (assignment) ->
      expressionStatement = t.expressionStatement assignment
      nodesToSkip.add expressionStatement
      expressionStatement

  wrapInClosure = (path) ->
    {node} = path
    func = t.functionExpression null, [], blockWrap [node]
    iife = t.callExpression func, []
    path.replaceWith iife

  del = (object, key) ->
    value = object[key]
    delete object[key]
    value

  visitFor = (path, {scope}) ->
    {node: {body: bodyOriginal, source, name, returns, style}, node} = path

    node.body = body = blockWrap [bodyOriginal]

    if returns
      returnsVariableName = scope.freeVariable 'results'
      returnsVariableIdentifier = t.identifier returnsVariableName
      makeReturn path.get('body'), resultsVariableName: returnsVariableName

    definitions = []
    sourceReference = source
    if name and not t.isIdentifier source
      sourceReferenceName = scope.freeVariable 'ref'
      sourceReference = t.identifier sourceReferenceName
      definitions.push t.expressionStatement t.assignmentExpression(
        '='
        sourceReference
        source
      )

    forStatement =
      if style is 'from'
        t.forOfStatement name, sourceReference, body
      else
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
          t.memberExpression sourceReference, t.identifier 'length'
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
              t.memberExpression sourceReference, indexVariableIdentifier, yes
            )
          )
        )

        t.forStatement(
          init
          test
          update
          body
        )

    if returns
      path.replaceWithMultiple [
        ...definitions
        t.expressionStatement t.assignmentExpression(
          '='
          returnsVariableIdentifier
          t.arrayExpression []
        )
        forStatement
        t.returnStatement returnsVariableIdentifier
      ]
    else
      path.replaceWithMultiple [
        ...definitions
        forStatement
      ]
  
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
    'FunctionExpression|ArrowFunctionExpression':
      enter: (path, state) ->
        {scope} = state
        newScope = new Scope scope
        newScope.shared = del state, 'sharedScope'
        state.scope = newScope

        makeReturn path.get 'body'
      exit: (path, state) ->
        {scope} = state
        addVariableDeclarations(path, scope) if scope.hasDeclaredVariables()
        state.scope = scope.parent

    'Statement|For': (path, state) ->
      unless path.node.type in ['Program', 'BlockStatement'] or path.parentPath.node.type in ['Program', 'BlockStatement']
        wrapInClosure path
        state.sharedScope = yes
        return

      visitFor(path, state) if path.node.type is 'For'

    ExpressionStatement: (path) ->
      {node} = path
      path.skip() if nodesToSkip.has node

    AssignmentExpression: (path, {scope}) ->
      {node: {left}} = path

      if left.type is 'ArrayPattern' and not getIsAssignable(left)
        handleDestructuringAssignment(path, scope)

    InterpolatedRegExpLiteral: (path) ->
      {node: {interpolatedPattern}} = path

      regExpCall = t.callExpression(
        t.identifier 'RegExp'
        [interpolatedPattern]
      )
      path.replaceWith regExpCall
  )

module.exports = transformer
