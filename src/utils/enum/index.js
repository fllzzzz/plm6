import { key2val, toArr, getBits, getBitsSum, setEnumValue } from './base'
// 注意modules文件夹下的文件（不相同的两个js）的export不能出现相同的名称

const modulesFiles = import.meta.globEager('./modules/*.js')

const modules = {}

// 遍历文件
for (const key in modulesFiles) {
  // TODO: 待修改，由export default取值改为从export中取值,避免导出重复写两遍
  modules[key.replace(/(\.\/modules\/|\.js)/g, '')] = modulesFiles[key].default
}
let exports = {}
exports = Object.assign(exports, modules)
// exports.default = {
//   key2val, toArr, getBits, setEnumValue
// }
export default {
  key2val, toArr, getBits, getBitsSum, setEnumValue
}

Object.defineProperty(exports, '__esModule', {
  value: true
})
