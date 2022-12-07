import request from '@/utils/request'

export function get(params) {
  return request({
    module: 'mes',
    url: 'layingOff',
    method: 'get',
    params
  })
}

export function getLayingWay(params) {
  return request({
    module: 'mes',
    url: 'cut/list/cut',
    method: 'get',
    params
  })
}

export function add(data) {
  return request({
    module: 'mes',
    url: 'layingOff',
    method: 'post',
    data: [data]
  })
}

export function batchAdd(data) {
  return request({
    module: 'mes',
    url: 'layingOff',
    method: 'post',
    data
  })
}

export function edit(data) {
  return request({
    module: 'mes',
    url: 'layingOff',
    method: 'put',
    data
  })
}

export function del(ids) {
  return request({
    module: 'mes',
    url: 'layingOff',
    method: 'delete',
    data: ids
  })
}

export default { get, add, batchAdd, del, edit }
