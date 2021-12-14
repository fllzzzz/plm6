import request from '@/utils/request'

export function get(params) {
  return request({
    module: 'plan',
    url: 'machinePart/listByCondition',
    method: 'get',
    params
  })
}

export default { get }
