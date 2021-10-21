import { createStore } from 'vuex'
import getters from './getters'

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
