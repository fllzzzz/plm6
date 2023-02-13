import request from '@/utils/request'

// 业财报表
export function get(params) {
  return request({
    module: '',
    url: '/api/contract/project-finance',
    method: 'get',
    params
  })
}

export default { get }
