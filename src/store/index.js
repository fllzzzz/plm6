import { createStore } from 'vuex'
import getters from './getters'

// https://webpack.js.org/guides/dependency-management/#requirecontext
// const modulesFiles = require.context('./modules', true, /\.js$/)

// you do not need `import app from './modules/app'`
// it will auto require all vuex module from modules file
// const modules = modulesFiles.keys().reduce((modules, modulePath) => {
//   // set './app.js' => 'app'
//   const moduleName = modulePath.replace(/^\.\/(.*)\.\w+$/, '$1')
//   const value = modulesFiles(modulePath)
//   modules[moduleName] = value.default
//   return modules
// }, {})

// const modules = modulesFiles.keys().reduce((modules, modulePath => {
//   const moduleName = modulePath.replace(/(\.\/module\/|\.js)/g, '')
//   modules[moduleName] = modulesFiles[moduleName].default
//   return modules
// }), {})

// 批量引入文件
const modulesFiles = import.meta.globEager('./modules/*.js')

const modules = {}

// 遍历文件
for (const key in modulesFiles) {
  modules[key.replace(/(\.\/modules\/|\.js)/g, '')] = modulesFiles[key].default
}

// 使其成为带命名空间的模块。保证在变量名一样的时候，添加一个父级名拼接。
Object.keys(modules).forEach(item => {
  modules[item]['namespaced'] = true
})

const store = createStore({
  modules,
  getters
})

export default store
