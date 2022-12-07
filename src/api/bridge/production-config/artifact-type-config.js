import request from '@/utils/request'

export function get(params) {
  return request({
    module: 'bridge',
    url: 'rivetWeld/box/type',
    method: 'get',
    params
  })
}

export function add(data) {
  return request({
    module: 'bridge',
    url: 'rivetWeld/box/type',
    method: 'post',
    data
  })
}

export function edit(data) {
  return request({
    module: 'bridge',
    url: 'rivetWeld/box/type',
    method: 'put',
    data
  })
}

export function del(ids) {
  return request({
    module: 'bridge',
    url: 'rivetWeld/box/type',
    method: 'delete',
    data: ids
  })
}

export default { get, add, del, edit }
