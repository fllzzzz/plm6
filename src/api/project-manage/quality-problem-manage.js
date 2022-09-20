import request from '@/utils/request'

export function get(params) {
  return request({
    module: 'project',
    url: `qhse-problem`,
    method: 'get',
    params
  })
}

// 整改记录
export function problemChangeLog(params) {
  return request({
    module: 'project',
    url: `qhse-change`,
    method: 'get',
    params
  })
}

// 整改审核
export function problemAudit(data) {
  return request({
    module: 'project',
    url: `qhse-change/check`,
    method: 'put',
    data
  })
}

export function download(id) {
  return request({
    module: 'project',
    url: `qhse-problem/print/${id}`,
    method: 'get',
    responseType: 'blob'
  })
}

// 关闭问题
export function close(id) {
  return request({
    module: 'project',
    url: `qhse-problem/closeById/${id}`,
    method: 'put'
  })
}
export default { get }
