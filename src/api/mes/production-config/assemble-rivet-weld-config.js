import request from '@/utils/request'

// 获取组铆焊工序列表
export function getProcess(params) {
  return request({
    module: 'mes',
    url: 'process/rivetWeld/assemble',
    method: 'get',
    params
  })
}

// 获取工序列表
export function getAssembleProcess(params) {
  return request({
    module: 'mes',
    url: 'productProcess/assemble/list',
    method: 'get',
    params
  })
}

export function get(params) {
  return request({
    module: 'mes',
    url: 'rivetWeld/assemble',
    method: 'get',
    params
  })
}

export function add(data) {
  return request({
    module: 'mes',
    url: 'rivetWeld/assemble',
    method: 'post',
    data: [data]
  })
}

export function batchAdd(data) {
  return request({
    module: 'mes',
    url: 'rivetWeld/assemble',
    method: 'post',
    data
  })
}

export function edit(data) {
  return request({
    module: 'mes',
    url: 'rivetWeld/assemble',
    method: 'put',
    data
  })
}

export function del(ids) {
  return request({
    module: 'mes',
    url: 'rivetWeld/assemble',
    method: 'delete',
    data: ids
  })
}

export default { get, del, add, batchAdd, edit }
