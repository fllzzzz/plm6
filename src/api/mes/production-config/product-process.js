import request from '@/utils/request'

export function get(params) {
  return request({
    module: 'mes',
    url: 'productProcess/page',
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

export function editWageQuota({ id, processId, productProcessId, areaPice, lengthPrice, weightPrice, wageQuotaType }) {
  return request({
    module: 'mes',
    url: 'wageQuota',
    method: 'put',
    data: { id, processId, productProcessId, areaPice, lengthPrice, weightPrice, wageQuotaType }
  })
}

export default { get, add, edit, del }
