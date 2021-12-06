import request from '@/utils/request'

export function get(params) {
  return request({
    module: 'plan',
    url: 'artifact/listByCondition',
    method: 'get',
    params
  })
}

export default { get }
