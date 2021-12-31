import request from '@/utils/request'

export function get(params) {
  return request({
    module: 'plan',
    url: `planDetail/summary`,
    method: 'get',
    params
  })
}

export default { get }
