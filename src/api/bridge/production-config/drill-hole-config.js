import request from '@/utils/request'

export function get(params) {
  return request({
    module: 'bridge',
    url: 'borehole',
    method: 'get',
    params
  })
}

export function add(data) {
  return request({
    module: 'bridge',
    url: 'borehole',
    method: 'post',
    data: [data]
  })
}

export function batchAdd(data) {
  return request({
    module: 'bridge',
    url: 'borehole',
    method: 'post',
    data
  })
}

export function edit(data) {
  return request({
    module: 'bridge',
    url: 'borehole',
    method: 'put',
    data
  })
}

export function del(ids) {
  return request({
    module: 'bridge',
    url: 'borehole',
    method: 'delete',
    data: ids
  })
}

export default { get, add, batchAdd, del, edit }
