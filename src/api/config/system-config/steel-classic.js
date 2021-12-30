import request from '@/utils/request'

export function get(params) {
  return request({
    url: '/api/steel/classification',
    method: 'get',
    params
  })
}

export function add(data) {
  return request({
    url: '/api/steel/classification',
    method: 'post',
    data
  })
}

export function edit(data) {
  return request({
    url: '/api/steel/classification',
    method: 'put',
    data
  })
}

export function del(ids) {
  return request({
    url: '/api/steel/classification',
    method: 'delete',
    data: ids
  })
}

export default { get, add, edit, del }
