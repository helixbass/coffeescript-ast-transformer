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

transformer = ({types: t}) ->
  visitor: withNullReturnValues(
    Program: ->
  )

module.exports = transformer
