import request from '@/utils/request'

export function get(params) {
  return request({
    module: 'contract',
    url: 'contract/payment/listPaySum',
    method: 'get',
    params
  })
}

export default { get }
