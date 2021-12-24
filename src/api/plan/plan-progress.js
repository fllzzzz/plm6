import request from '@/utils/request'

export function get(params) {
  return request({
    module: 'plan',
    url: `planDetail/listTrace`,
    method: 'get',
    params
  })
}

export default { get }
