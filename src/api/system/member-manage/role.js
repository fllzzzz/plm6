import request from '@/utils/request'

export function get(params) {
  return request({
    module: 'system',
    url: 'role',
    method: 'get',
    params
  })
}

export function add(data) {
  return request({
    module: 'system',
    url: 'role',
    method: 'post',
    data
  })
}

export function edit(data) {
  return request({
    module: 'system',
    url: 'role',
    method: 'put',
    data
  })
}

export function del(ids) {
  return request({
    module: 'system',
    url: 'role',
    method: 'delete',
    data: ids
  })
}

export function bindMenu(data) {
  return request({
    module: 'system',
    url: 'role/menu',
    method: 'put',
    data
  })
}

export function roleAll() {
  return request({
    module: 'system',
    url: 'role/all',
    method: 'get'
  })
}

export default { add, edit, del, get }
