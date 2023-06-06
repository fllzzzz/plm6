import request from '@/utils/request'

// 获取组铆焊工序列表
export function getProcess(params) {
  return request({
    module: 'bridge',
    url: 'process/rivetWeld/element',
    method: 'get',
    params
  })
}

// 获取工序列表
export function getAssembleProcess(params) {
  return request({
    module: 'bridge',
    url: 'productProcess/element/list',
    method: 'get',
    params
  })
}

export function get(params) {
  return request({
    module: 'bridge',
    url: 'rivetWeld/element',
    method: 'get',
    params
  })
}

export function add(data) {
  return request({
    module: 'bridge',
    url: 'rivetWeld/element',
    method: 'post',
    data: [data]
  })
}

export function batchAdd(data) {
  return request({
    module: 'bridge',
    url: 'rivetWeld/element',
    method: 'post',
    data
  })
}

export function edit(data) {
  return request({
    module: 'bridge',
    url: 'rivetWeld/element',
    method: 'put',
    data
  })
}

export function del(ids) {
  return request({
    module: 'bridge',
    url: 'rivetWeld/element',
    method: 'delete',
    data: ids
  })
}

export default { get, del, add, batchAdd, edit }
