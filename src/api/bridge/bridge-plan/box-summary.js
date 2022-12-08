import request from '@/utils/request'

export function get(params) {
  return request({
    url: '/api/bridge/tech-box/list-summary',
    method: 'get',
    params
  })
}

// 获取总重量
export function boxTotalWeight(params) {
  return request({
    url: '/api/bridge/tech-box/get-sum',
    method: 'get',
    params
  })
}

export default { get }
