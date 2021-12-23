import request from '@/utils/request'

export function get(params) {
  return request({
    module: 'contract',
    url: 'contract/reimbursement/listPage',
    method: 'get',
    params
  })
}

export function add(data) {
  return request({
    module: 'contract',
    url: 'contract/reimbursement/save',
    method: 'post',
    data
  })
}

export function edit(data) {
  return request({
    module: 'contract',
    url: 'contract/reimbursement/update',
    method: 'put',
    data
  })
}

// 审核
export function editStatus(data) {
  return request({
    module: 'contract',
    url: `contract/reimbursement/confirm`,
    method: 'put',
    data
  })
}

export function reimbursementSum() {
  return request({
    module: 'contract',
    url: 'contract/reimbursement/getSumPayAmount',
    method: 'get'
  })
}

export default { get, add, edit }
