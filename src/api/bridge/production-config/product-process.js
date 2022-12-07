import request from '@/utils/request'

export function get(params) {
  return request({
    module: 'bridge',
    url: 'productProcess/page',
    method: 'get',
    params
  })
}

// 列表-分段(箱体)种类工序
export function getBox(params) {
  return request({
    module: 'bridge',
    url: 'productProcess/box',
    method: 'get',
    params
  })
}

// 列表-单元种类工序
export function getCell(params) {
  return request({
    module: 'bridge',
    url: 'productProcess/element',
    method: 'get',
    params
  })
}

// 列表-零件种类工序
export function getMachinePart(params) {
  return request({
    module: 'bridge',
    url: 'productProcess/machine_part',
    method: 'get',
    params
  })
}

export function add(data) {
  return request({
    module: 'bridge',
    url: 'productProcess',
    method: 'post',
    data
  })
}

export function edit(data) {
  return request({
    module: 'bridge',
    url: 'productProcess',
    method: 'put',
    data
  })
}

export function editStatus(data) {
  return request({
    module: 'bridge',
    url: 'productProcess/changeState',
    method: 'put',
    data
  })
}

export function del(ids) {
  return request({
    module: 'bridge',
    url: 'productProcess',
    method: 'delete',
    data: { ids }
  })
}

export function editWageQuota({ id, processId, productProcessId, price }) {
  return request({
    module: 'bridge',
    url: 'wageQuota',
    method: 'put',
    data: { id, processId, productProcessId, price }
  })
}

export default { get, add, edit, del }
