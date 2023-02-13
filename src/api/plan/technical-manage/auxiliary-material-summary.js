import request from '@/utils/request'

export function get(params) {
  return request({
    module: 'plan',
    url: `auxiliaryPart`,
    method: 'get',
    params
  })
}

export default { get }
