import request from '@/utils/request'

export function get(params) {
  return request({
    module: 'plan',
    url: 'area',
    method: 'get',
    params
  })
}

export function add(data) {
  return request({
    module: 'plan',
    url: 'area',
    method: 'post',
    data
  })
}

export function edit(data) {
  return request({
    module: 'plan',
    url: 'area',
    method: 'put',
    data
  })
}

export function del(ids) {
  return request({
    module: 'plan',
    url: 'area',
    method: 'delete',
    data: ids
  })
}

export default { add, edit, del, get }
