import request from '@/utils/request'

export function get(params) {
  return request({
    module: 'contract',
    url: 'contract/invoice/project',
    method: 'get',
    params
  })
}

// export function add(data) {
//   return request({
//     module: 'contract',
//     url: 'contract/invoice/save',
//     method: 'post',
//     data
//   })
// }

export function add(data) {
  return request({
    module: 'contract',
    url: 'contract/invoice/project',
    method: 'post',
    params: { projectId: data.projectId },
    data: data.list
  })
}

export function edit(data) {
  return request({
    module: 'contract',
    url: 'contract/invoice/project',
    method: 'post',
    data
  })
}

// 审核
export function editStatus(data) {
  return request({
    module: 'contract',
    url: `contract/invoice/audit/project`,
    method: 'put',
    data
  })
}

export function del(id) {
  return request({
    module: 'contract',
    url: `contract/invoice/deleteById/${id}/project`,
    method: 'delete'
  })
}

export function getTotalNumber(params) {
  return request({
    module: 'contract',
    url: 'contract/invoice/getInvoiceSum',
    method: 'get',
    params
  })
}

export default { get, add, edit, del }
