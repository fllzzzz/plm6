import request from '@/utils/request'

export function get(params) {
  return request({
    module: 'contract',
    url: `contract/cargoListLedger/listInvoice`,
    method: 'get',
    params,
    cancelKey: false
  })
}

export function add(data) {
  return request({
    module: 'contract',
    url: 'contract/cargoListLedger/invoice',
    method: 'post',
    data: [data]
  })
}

export function edit(data) {
  return request({
    module: 'contract',
    url: 'contract/cargoListLedger/invoice',
    method: 'put',
    data: [data]
  })
}

export function del(data) {
  return request({
    module: 'contract',
    url: 'contract/cargoListLedger/invoice',
    method: 'delete',
    data
  })
}

export function audit(id, auditStatus) {
  return request({
    module: 'contract',
    url: `contract/cargoListLedger/invoice/audit/${id}/${auditStatus}`,
    method: 'put'
  })
}
export default { get, add, edit, del }
