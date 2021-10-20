import { createApp } from 'vue'
import App from './App.vue'
import router from './router'
import store from './store'

// 访问权限控制
import '@/permission'

// js基础类型原型链方法修改
import '@/utils/js-datatypes-prototype'

// 组件：element-plus | element-ui 不支持 vue3
import useElementPlus from '@/plugins/element-plus'
import 'element-plus/packages/theme-chalk/src/base.scss'

// 组件：svg-icon
import useSvgIcon from '@/plugins/svg-icon'

// 设置全局方法
import setGlobalFun from '@/plugins/globalFun'

// 系统定义的全局组件
import useGlobalComponents from '@/plugins/global-component'

const app = createApp(App)

useElementPlus(app)
useSvgIcon(app)
useGlobalComponents(app)
setGlobalFun(app)

app.use(router)
app.use(store)
app.mount('#app')
