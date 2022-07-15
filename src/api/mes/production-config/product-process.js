import request from '@/utils/request'

export function get(params) {
  return request({
    module: 'mes',
    url: 'productProcess/page',
    method: 'get',
    params
  })
}

// 列表-构件种类工序
export function getArtifact(params) {
  return request({
    module: 'mes',
    url: 'productProcess/artifact',
    method: 'get',
    params
  })
}

// 列表-部件种类工序
export function getAssemble(params) {
  return request({
    module: 'mes',
    url: 'productProcess/assemble',
    method: 'get',
    params
  })
}

// 列表-零件种类工序
export function getMachinePart(params) {
  return request({
    module: 'mes',
    url: 'productProcess/machine_part',
    method: 'get',
    params
  })
}

export function add(data) {
  return request({
    module: 'mes',
    url: 'productProcess',
    method: 'post',
    data
  })
}

export function edit(data) {
  return request({
    module: 'mes',
    url: 'productProcess',
    method: 'put',
    data
  })
}

export function editStatus(data) {
  return request({
    module: 'mes',
    url: 'productProcess/changeState',
    method: 'put',
    data
  })
}

export function del(ids) {
  return request({
    module: 'mes',
    url: 'productProcess',
    method: 'delete',
    data: { ids }
  })
}

export function editWageQuota({ id, processId, productProcessId, price }) {
  return request({
    module: 'mes',
    url: 'wageQuota',
    method: 'put',
    data: { id, processId, productProcessId, price }
  })
}

export default { get, add, edit, del }
