import { isNotBlank, isBlank } from '@data-type/index'

// 全局方法
export default (app) => {
  app.config.globalProperties.$isNotBlank = isNotBlank
  app.config.globalProperties.$isBlank = isBlank
}
