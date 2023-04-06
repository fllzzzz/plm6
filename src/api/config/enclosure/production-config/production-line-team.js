import request from '@/utils/request'

/**
 * 生产线：班组列表（不分页）
 * @param {number} factoryId|required 工厂id
 */
export function productionTeam(params) {
  return request({
    url: '/api/mes/enclosure/team',
    method: 'get',
    params
  })
}

/**
 * 生产线：质检班组列表（不分页）
 * @param {number} factoryId|required 工厂id
 */
export function inspectionTeam(params) {
  return request({
    url: '/api/mes/enclosure/inspectionTeam',
    method: 'get',
    params
  })
}
