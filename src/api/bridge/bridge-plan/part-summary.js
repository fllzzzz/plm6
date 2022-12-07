import request from '@/utils/request'

export function get(params) {
  return request({
    url: '/api/bridge/tech-machinePart/list-summary',
    method: 'get',
    params
  })
}

// 获取总重量
export function partTotalWeight(params) {
  return request({
    url: '/api/bridge/tech-machinePart/get-sum',
    method: 'get',
    params
  })
}

export default { get }
