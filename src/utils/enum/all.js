// import { key2val, toArr, getBits, setEnumValue } from './base'
// 注意modules文件夹下的文件（不相同的两个js）的export不能出现相同的名称

// const modulesFiles = require.context('./modules', true, /\.js$/)

const modulesFiles = import.meta.globEager('./modules/*.js')

const modules = {}

for (const key in modulesFiles) {
  // TODO: 待修改，由export default取值改为从export中取值,避免导出重复写两遍
  modules[key.replace(/(\.\/modules\/|\.js)/g, '')] = modulesFiles[key].default
}

// const modules = modulesFiles.keys().reduce((modules, modulePath) => {
//   const value = modulesFiles(modulePath)
//   modules = Object.assign(modules, { ...value.default })
//   return modules
// }, { key2val, toArr, getBits, setEnumValue })

// exports = Object.assign(exports, modules)
// exports.default = {
//   key2val, toArr, getBits, setEnumValue
// }
// exports.__esModule = true
export default modules
