import { val2key, toArr, getBits, getBitsSum, setEnumValue } from './base'
// 注意modules文件夹下的文件（不相同的两个js）的export不能出现相同的名称

// const modulesFiles = require.context('./modules', true, /\.js$/)

// const modules = modulesFiles.keys().reduce((modules, modulePath) => {
//   const value = modulesFiles(modulePath)
//   modules = Object.assign(modules, { ...value.default })
//   return modules
// }, { val2key, toArr, getBits, setEnumValue })

const modulesFiles = import.meta.globEager('./modules/*.js')

const modules = {}

// 遍历文件
for (const key in modulesFiles) {
  modules[key.replace(/(\.\/modules\/|\.js)/g, '')] = modulesFiles[key].default
}
let exports = {}
exports = Object.assign(exports, modules)
// exports.default = {
//   val2key, toArr, getBits, setEnumValue
// }
export default {
  val2key, toArr, getBits, getBitsSum, setEnumValue
}
// Object.defineProperty(exports, 'default', {
//   val2key, toArr, getBits, setEnumValue
// })

Object.defineProperty(exports, '__esModule', {
  value: true
})
