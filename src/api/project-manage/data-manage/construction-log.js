import request from '@/utils/request'

export function get(params) {
  return request({
    module: 'project',
    url: `construction-log`,
    method: 'get',
    params
  })
}

export function add(data) {
  return request({
    module: 'project',
    url: `construction-log`,
    method: 'post',
    data
  })
}

export function edit(data) {
  return request({
    module: 'project',
    url: `construction-log`,
    method: 'put',
    data
  })
}

export function download(params) {
  return request({
    module: 'project',
    url: `construction-log/export`,
    method: 'get',
    params,
    responseType: 'blob'
  })
}

export default { get, add, edit }
