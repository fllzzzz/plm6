import request from '@/utils/request'

export function get(params) {
  return request({
    module: 'user',
    url: 'job',
    method: 'get',
    params
  })
}

export function add(data) {
  return request({
    module: 'user',
    url: 'job',
    method: 'post',
    data
  })
}

export function edit(data) {
  return request({
    module: 'user',
    url: 'job',
    method: 'put',
    data
  })
}

export function del(ids) {
  return request({
    module: 'user',
    url: 'job',
    method: 'delete',
    data: ids
  })
}

export function editStatus(params) {
  return request({
    module: 'user',
    url: 'job/enabled',
    method: 'put',
    params
  })
}

export function jobAll(params) {
  return request({
    module: 'user',
    url: 'job/all',
    method: 'get',
    params
  })
}

export default { add, edit, del, get }