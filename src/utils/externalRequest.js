/**
 * 接口请求工具类
 */
import axios from 'axios'
import store from '@/store'
import { isBlank } from '@data-type/index'
import { getToken } from '@/utils/storage'
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

// request拦截器
// 注意：Map对象无法提交，会被转化为空对象
service.interceptors.request.use(
  config => {
    // 设置token
    if (getToken()) {
      config.headers['Authorization'] = getToken()
    }
    if (config.headers['Content-Type'] === 'multipart/form-data') {
      if (config.data === null || config.data === undefined) config.data = {}
      if (config.params === null || config.params === undefined) config.params = {}
    } else {
      if (isBlank(config.data)) config.data = {}
      if (isBlank(config.params)) config.params = {}
    }
    // 格式化数据
    if (config.headers['Content-Type'] === 'application/x-www-form-urlencoded') {
      config.data = Qs.stringify(config.data)
    }

    if (config.method === 'get') {
      config.paramsSerializer = (params) => {
        return Qs.stringify(params, { arrayFormat: 'repeat' })
      }
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
    // console.log(error.message) // for debug
    return Promise.reject(error)
  }
)

export default service

