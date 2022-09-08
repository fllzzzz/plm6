import request from '@/utils/request'

// 切割配置列表/下料方式配置

export function batchUnloadingAdd(data) {
  return request({
    module: 'mes',
    url: 'cut/list/laying',
    method: 'post',
    data
  })
}
export function batchUnloading() {
  return request({
    module: 'mes',
    url: 'cut/list/laying',
    method: 'get'
  })
}

export default { batchUnloading, batchUnloadingAdd }
