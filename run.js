#!/usr/bin/env node

const fs = require('fs')
const util = require('util')
const coffeescript = require('coffeescript')
const {default: generate} = require('@babel/generator')
const {omit, mapValues, flow, map} = require('lodash/fp')
const {isPlainObject, isArray} = require('lodash')

const {getTransformedAst} = require('./lib')

const [,, inputFilename, command] = process.argv

const stripLocationDataAndTokens = (obj) =>
  isArray(obj)
    ? map(stripLocationDataAndTokens)(obj)
    : isPlainObject(obj)
    ? flow(
        omit(['tokens', 'loc', 'range', 'start', 'end']),
        mapValues(stripLocationDataAndTokens)
      )(obj)
    : obj

const dump = (obj) => console.log(util.inspect(obj, false, null))

const inputSource = fs.readFileSync(inputFilename, 'utf-8')
const inputAst = coffeescript.compile(inputSource, {ast: true})
if (command === 'coffee-ast') {
  dump(stripLocationDataAndTokens(inputAst))
  return
}
const transformedAst = getTransformedAst(inputAst)
if (command === 'js') {
  const {code: printed} = generate(transformedAst)
  console.log(printed)
  return
}
dump(transformedAst)
