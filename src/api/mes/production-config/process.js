import request from '@/utils/request'

export function get(params) {
  return request({
    module: 'mes',
    url: 'process/page',
    method: 'get',
    params
  })
}

export function add(data) {
  return request({
    module: 'mes',
    url: 'process',
    method: 'post',
    data
  })
}

export function edit(data) {
  return request({
    module: 'mes',
    url: 'process',
    method: 'put',
    data
  })
}

export function del(ids) {
  return request({
    module: 'mes',
    url: 'process',
    method: 'delete',
    data: { ids }
  })
}

export default { get, add, edit, del }
