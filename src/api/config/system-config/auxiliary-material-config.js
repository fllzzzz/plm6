import request from '@/utils/request'

export function get(params) {
  return request({
    module: 'contract',
    url: 'system/auxiliary-class',
    method: 'get',
    params
  })
}

export function add(data) {
  return request({
    module: 'contract',
    url: 'system/auxiliary-class',
    method: 'post',
    data
  })
}

export function edit(data) {
  return request({
    module: 'contract',
    url: 'system/auxiliary-class',
    method: 'put',
    data
  })
}

export function del(id) {
  return request({
    module: 'contract',
    url: `system/auxiliary-class/delById/${id}`,
    method: 'delete'
  })
}

export default { get, add, edit, del }
