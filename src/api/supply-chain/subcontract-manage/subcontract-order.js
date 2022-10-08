import request from '@/utils/request'

export function get(params) {
  return request({
    module: 'contract',
    url: `scm/subcontract-order`,
    method: 'get',
    params
  })
}

export function add(data) {
  return request({
    module: 'contract',
    url: 'scm/subcontract-order',
    method: 'post',
    data
  })
}

export function edit(data) {
  return request({
    module: 'contract',
    url: `scm/subcontract-order`,
    method: 'put',
    data
  })
}

export function del(ids) {
  return request({
    module: 'contract',
    url: 'scm/subcontract-order',
    method: 'delete',
    data: ids
  })
}

// 分包订单汇总
export function subContractSummary(params) {
  return request({
    module: 'contract',
    url: `scm/subcontract-order/getSum`,
    method: 'get'
  })
}

export default { add, edit, del, get }
