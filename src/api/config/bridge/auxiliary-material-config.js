import request from '@/utils/request'

export function get(params) {
  return request({
    url: '/api/bridge/auxiliary-class',
    method: 'get',
    params
  })
}

export function add(data) {
  return request({
    url: '/api/bridge/auxiliary-class',
    method: 'post',
    data
  })
}

export function edit(data) {
  return request({
    url: '/api/bridge/auxiliary-class',
    method: 'put',
    data
  })
}

export function del(id) {
  return request({
    url: `/api/bridge/auxiliary-class/delById/${id}`,
    method: 'delete'
  })
}

export default { get, add, edit, del }
