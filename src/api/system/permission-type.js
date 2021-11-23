import request from '@/utils/request'

export function get(params) {
  return request({
    module: 'system',
    url: 'permission/type',
    method: 'get',
    params
  })
}

export function add(data) {
  return request({
    module: 'system',
    url: 'permission/type',
    method: 'post',
    data
  })
}

export function edit(data) {
  return request({
    module: 'system',
    url: 'permission/type',
    method: 'put',
    data
  })
}

export function del(ids) {
  return request({
    module: 'system',
    url: 'permission/type',
    method: 'delete',
    data: ids
  })
}

export function permissionTypeAll(params) {
  return request({
    module: 'system',
    url: 'permission/type',
    method: 'get',
    params
  })
}

export default { add, edit, del, get }
