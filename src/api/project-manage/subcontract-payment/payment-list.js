import request from '@/utils/request'

export function get(params) {
  return request({
    module: 'project',
    url: `sub-payment/list-subOrder`,
    method: 'get',
    params,
    cancelKey: false
  })
}

export default { get }
