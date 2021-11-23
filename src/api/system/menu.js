import request from '@/utils/request'

export function get(params) {
  return request({
    module: 'system',
    url: 'menu',
    method: 'get',
    params
  })
}

export function add(data) {
  return request({
    module: 'system',
    url: 'menu',
    method: 'post',
    data
  })
}

export function edit(data) {
  return request({
    module: 'system',
    url: 'menu',
    method: 'put',
    data
  })
}

export function del(ids) {
  return request({
    module: 'system',
    url: 'menu',
    method: 'delete',
    data: ids
  })
}

export function menuTree() {
  return request({
    module: 'system',
    url: 'menu/tree',
    method: 'get'
  })
}

export default { add, edit, del, get }
