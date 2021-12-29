import request from '@/utils/request'

export function get(params) {
  return request({
    url: '/api/steel/analysis',
    method: 'get',
    params
  })
}

export default { get }
