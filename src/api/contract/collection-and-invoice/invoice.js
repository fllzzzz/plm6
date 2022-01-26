import request from '@/utils/request'

export function get(params) {
  return request({
    module: 'contract',
    url: 'contract/invoice/listPage',
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
    url: 'contract/invoice/saveList',
    method: 'post',
    params: { projectId: data.projectId },
    data: data.list
  })
}

export function edit(data) {
  return request({
    module: 'contract',
    url: 'contract/invoice/edit',
    method: 'post',
    data
  })
}

// 审核
export function editStatus(id, status) {
  return request({
    module: 'contract',
    url: `contract/invoice/audit/invoiceId/${id}/status/${status}`,
    method: 'put'
  })
}

export function del(id) {
  return request({
    module: 'contract',
    url: `contract/invoice/deleteById/${id}`,
    method: 'delete'
  })
}

export default { get, add, edit, del }
