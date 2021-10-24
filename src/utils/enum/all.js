import { key2val, toArr, getBits, setEnumValue } from './base'
// 注意modules文件夹下的文件（不相同的两个js）的export不能出现相同的名称

const modulesFiles = require.context('./modules', true, /\.js$/)

const modules = modulesFiles.keys().reduce((modules, modulePath) => {
  const value = modulesFiles(modulePath)
  modules = Object.assign(modules, { ...value.default })
  return modules
}, { key2val, toArr, getBits, setEnumValue })

// exports = Object.assign(exports, modules)
// exports.default = {
//   key2val, toArr, getBits, setEnumValue
// }
// exports.__esModule = true
export default modules
