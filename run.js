#!/usr/bin/env node

const fs = require('fs')
const util = require('util')
const coffeescript = require('coffeescript')
const {default: generate} = require('@babel/generator')

const {getTransformedAst} = require('./lib')

const [,, command] = process.argv

const dump = (obj) => console.log(util.inspect(obj, false, null))

const inputSource = fs.readFileSync('./tmp.coffee', 'utf-8')
const inputAst = coffeescript.compile(inputSource, {ast: true})
const transformedAst = getTransformedAst(inputAst)
if (command === 'js') {
  const {code: printed} = generate(transformedAst)
  console.log(printed)
  return
}
dump(transformedAst)
