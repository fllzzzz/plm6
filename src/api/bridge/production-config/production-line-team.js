import request from '@/utils/request'

export function get(params) {
  return request({
    url: '/api/mes/bridge/team/page',
    method: 'get',
    params
  })
}

export function add(data) {
  return request({
    url: '/api/mes/bridge/team',
    method: 'post',
    data
  })
}

export function edit(data) {
  return request({
    url: '/api/mes/bridge/team',
    method: 'put',
    data
  })
}

export function del(ids) {
  return request({
    url: '/api/mes/bridge/team',
    method: 'delete',
    data: { ids }
  })
}

export default { get, add, edit, del }
