import store from '@/store'

// 基础路径

// 设置sse
export function setSSE(request, callback) {
  const baseApi = store.getters.baseApi

  if ('EventSource' in window) {
    let url = request.url
    if (request.module) {
      url = baseApi[request.module] + request.url
    }
    // const that = this
    const source = new EventSource(url, {
      withCredentials: true
    })
    source.onmessage = function (e) {
      console.log(e)
      if (typeof callback === 'function') {
        // callback()
      }
    }
    source.onopen = function (e) {}
    source.onerror = function (e) {
      if (e.readyState === EventSource.CLOSED) {
        console.log('关闭')
      } else {
        console.log('onerror:' + e.readyState)
      }
    }
  } else {
    throw Error('没有sse')
  }
}
