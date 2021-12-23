import request from '@/utils/request'

export function get(params) {
  return request({
    module: 'contract',
    url: 'contract/receive-invoice/listPage',
    method: 'get',
    params
  })
}

export function add(data) {
  return request({
    module: 'contract',
    url: 'contract/receive-invoice/save',
    method: 'post',
    data
  })
}

// 审核
export function editStatus(params) {
  return request({
    module: 'contract',
    url: `contract/receive-invoice/audit`,
    method: 'put',
    params
  })
}

export function receiveSum() {
  return request({
    module: 'contract',
    url: 'contract/receive-invoice/sum',
    method: 'get'
  })
}

export default { get, add }
