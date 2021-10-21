/**
 * 接口请求工具类
 */
import axios from 'axios'
import store from '@/store'
import { isBlank } from '@/utils/data-type'
import { getToken, getRequestUrl } from '@/utils/storage'
import { validRequestUrl } from '@/utils/validate'
import Qs from 'qs'
import { ElMessage, ElMessageBox } from 'element-plus'

// 创建axios实例
const service = axios.create({
  // api的base_url：null
  // baseURL: process.env.BASE_API,
  timeout: 10000, // 请求超时时间
  // Content-Type: 默认使用application/json; 如特殊接口后端有要求可以使用：application/x-www-form-urlencoded
  headers: { 'Content-Type': 'application/json' },
  // 接口取消的key值，true：表示可取消（true时使用url作为key）。也可赋值为其他类型，用来表示接口取消对应的key值
  cancelKey: true
})

// 取消请求
const CancelToken = axios.CancelToken
const axiosCancelTokens = store.getters.axiosCancelTokens
// 基础路径
const baseApi = store.getters.baseApi

// request拦截器
service.interceptors.request.use(
  config => {
    const requestUrl = getRequestUrl()
    // 未填写url默认请求域名所在的地址，因此不报错
    if (!requestUrl || validRequestUrl(requestUrl)) {
      config.baseURL = requestUrl
    } else {
      // TODO: 使用setTimeout的原因
      setTimeout(() => {
        ElMessageBox.confirm('您填写的公司地址无法正常访问，可以取消继续留在该页面，或者重新填写访问地址', '确定重写', {
          confirmButtonText: '重写地址',
          cancelButtonText: '取消',
          type: 'warning'
        }).then(() => {
          store.dispatch('user/resetRequestUrl')
          location.reload() // 为了重新实例化vue-router对象 避免bug
        })
      }, 500)
    }
    if (config.module) {
      config.url = baseApi[config.module] + config.url
    }

    // 设置token
    if (getToken()) {
      config.headers['Authorization'] = `cat ${getToken()}`
    }

    if (isBlank(config.data)) config.data = {}
    if (isBlank(config.params)) config.params = {}

    // 格式化数据
    if (config.headers['Content-Type'] === 'application/x-www-form-urlencoded') {
      config.data = Qs.stringify(config.data)
    }

    // 带cancelKey的接口把source保存下来，再次请求前需要取消之前的请求
    if (config.cancelKey) {
      // 当cancelKey 填写boolean值时，直接使用url作为key
      const cancelKey = typeof config.cancelKey === 'boolean' ? config.url : config.cancelKey

      // 取消接口
      if (axiosCancelTokens[cancelKey]) {
        axiosCancelTokens[cancelKey].cancel({ cancel: true, message: `取消了接口：${cancelKey}` })
      }

      // 生成新的cancelToken
      const source = CancelToken.source()
      config.cancelToken = source.token
      store.dispatch('interface/setAxiosCancelTokens', { cancelKey: cancelKey, source })
    }
    return config
  },
  error => {
    // Do something with request error
    console.log(error) // for debug
    Promise.reject(error)
  }
)

// response 拦截器
service.interceptors.response.use(
  response => {
    // 文件流直接返回response
    if (response.config.responseType === 'blob' || response.config.responseType === 'arraybuffer') {
      return response
    }

    // 非文件类型获取数据与状态码
    const res = response.data
    if (res.code === 20000) {
      return res.data
    } else {
      switch (res.code) {
        // 登录失效，重新登录
        case 40001:
        case 40002:
          ElMessageBox.confirm('你已被登出，可以取消继续留在该页面，或者重新登录', '确定登出', {
            confirmButtonText: '重新登录',
            cancelButtonText: '取消',
            type: 'warning'
          }).then(() => {
            store.dispatch('user/logout')
            location.reload() // 为了重新实例化vue-router对象 避免bug
          })
          break
        default:
          ElMessage({
            message: res.message,
            type: 'error',
            duration: 5 * 1000
          })
      }
      return Promise.reject('error')
    }
  },
  error => {
    console.log(error.message) // for debug
    return Promise.reject(error)
  }
)

export default service
