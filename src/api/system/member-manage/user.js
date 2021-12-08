import request from '@/utils/request'

export function get(params) {
  return request({
    module: 'system',
    url: 'user',
    method: 'get',
    params
  })
}

export function add(data) {
  return request({
    module: 'system',
    url: 'user',
    method: 'post',
    data
  })
}

export function edit(data) {
  return request({
    module: 'system',
    url: 'user',
    method: 'put',
    data
  })
}

export function del(ids) {
  return request({
    module: 'system',
    url: 'user',
    method: 'delete',
    data: ids
  })
}

export function editStatus(data) {
  return request({
    module: 'system',
    url: 'user/enabled',
    method: 'put',
    data
  })
}
export default { add, edit, del, get }
