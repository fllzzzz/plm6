import request from '@/utils/request'

// 查询构件种类配置列表
export function getRivetWeld(params) {
  return request({
    module: 'mes',
    url: 'rivetWeld/artifact/type/find',
    method: 'get',
    params
  })
}

// 获取组铆焊工序列表
export function getProcess(params) {
  return request({
    module: 'mes',
    url: 'process/rivetWeld',
    method: 'get',
    params
  })
}

// 获取构件类型与工序列表
export function getArtifactProcess(params) {
  return request({
    module: 'mes',
    url: 'productProcess/artifact/list',
    method: 'get',
    params
  })
}

export function get(params) {
  return request({
    module: 'mes',
    url: 'rivetWeld',
    method: 'get',
    params
  })
}

export function add(data) {
  return request({
    module: 'mes',
    url: 'rivetWeld',
    method: 'post',
    data: [data]
  })
}

export function batchAdd(data) {
  return request({
    module: 'mes',
    url: 'rivetWeld',
    method: 'post',
    data
  })
}

export function edit(data) {
  return request({
    module: 'mes',
    url: 'rivetWeld',
    method: 'put',
    data
  })
}

export function del(ids) {
  return request({
    module: 'mes',
    url: 'rivetWeld',
    method: 'delete',
    data: ids
  })
}

export default { get, del, add, batchAdd, edit }
