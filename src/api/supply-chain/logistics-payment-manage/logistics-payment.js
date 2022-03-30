import request from '@/utils/request'

export function get(params) {
  return request({
    module: 'contract',
    url: `supply-chain/logistics-payment/apply`,
    method: 'get',
    params
  })
}

export function add(data) {
  return request({
    module: 'contract',
    url: 'supply-chain/logistics-payment/apply',
    method: 'post',
    data
  })
}

export function edit(data) {
  return request({
    module: 'contract',
    url: `supply-chain/logistics-payment/apply`,
    method: 'put',
    data
  })
}

export function del(ids) {
  return request({
    module: 'contract',
    url: 'supply-chain/logistics-payment/apply',
    method: 'delete',
    data: ids
  })
}

export default { add, edit, del, get }
