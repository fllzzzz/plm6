import request from '@/utils/request'

export function get(params) {
  return request({
    url: '/api/bridge/tech-element/list-summary',
    method: 'get',
    params
  })
}

// 获取总重量
export function cellTotalWeight(params) {
  return request({
    url: '/api/bridge/tech-element/get-sum',
    method: 'get',
    params
  })
}

export default { get }
