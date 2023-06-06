import request from '@/utils/request'

// 查询分段种类配置列表
export function getRivetWeld(params) {
  return request({
    module: 'bridge',
    url: 'rivetWeld/box/type',
    method: 'get',
    params
  })
}

// 获取组铆焊工序列表
export function getProcess(params) {
  return request({
    module: 'bridge',
    url: 'process/rivetWeld',
    method: 'get',
    params
  })
}

// 获取单元类型与工序列表
export function getBoxProcess(params) {
  return request({
    module: 'bridge',
    url: 'productProcess/box/list',
    method: 'get',
    params
  })
}

export function get(params) {
  return request({
    module: 'bridge',
    url: 'rivetWeld',
    method: 'get',
    params
  })
}

export function add(data) {
  return request({
    module: 'bridge',
    url: 'rivetWeld',
    method: 'post',
    data: [data]
  })
}

export function batchAdd(data) {
  return request({
    module: 'bridge',
    url: 'rivetWeld',
    method: 'post',
    data
  })
}

export function edit(data) {
  return request({
    module: 'bridge',
    url: 'rivetWeld',
    method: 'put',
    data
  })
}

export function del(ids) {
  return request({
    module: 'bridge',
    url: 'rivetWeld',
    method: 'delete',
    data: ids
  })
}

export default { get, del, add, batchAdd, edit }
