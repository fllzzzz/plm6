import request from '@/utils/request'

export function get(params) {
  return request({
    module: 'bridge',
    url: 'rivetWeld/artifact/type',
    method: 'get',
    params
  })
}

export function add(data) {
  return request({
    module: 'bridge',
    url: 'rivetWeld/artifact/type',
    method: 'post',
    data
  })
}

export function edit(data) {
  return request({
    module: 'bridge',
    url: 'rivetWeld/artifact/type',
    method: 'put',
    data
  })
}

export function del(ids) {
  return request({
    module: 'bridge',
    url: 'rivetWeld/artifact/type',
    method: 'delete',
    data: ids
  })
}

export default { get, add, del, edit }
