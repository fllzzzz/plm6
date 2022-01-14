import request from '@/utils/request'

export function get(params) {
  return request({
    module: 'mes',
    url: 'produce_surplus/page',
    method: 'get',
    params
  })
}

export default { get }
