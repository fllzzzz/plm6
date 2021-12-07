import request from '@/utils/request'

export function get(params) {
  return request({
    module: 'mes',
    url: 'abnormal',
    method: 'get',
    params
  })
}

export default { get }
