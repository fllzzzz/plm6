import request from '@/utils/request'

export function get(params) {
  return request({
    module: 'contract',
    url: 'bridge/element-class',
    method: 'get',
    params
  })
}

export function add(data) {
  return request({
    module: 'contract',
    url: 'bridge/element-class',
    method: 'post',
    data
  })
}

export function edit(data) {
  return request({
    module: 'contract',
    url: 'bridge/element-class',
    method: 'put',
    data
  })
}

export function del(id) {
  return request({
    module: 'contract',
    url: `bridge/element-class/delById/${id}`,
    method: 'delete'
  })
}

export default { get, add, edit, del }
