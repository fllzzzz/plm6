import request from '@/utils/request'

/**
 * 生产线：班组列表（不分页）
 * @param {number} id|required 生产线id
 */
export function get(params) {
  return request({
    url: '/api/mes/enclosure/productionLine/teams',
    method: 'get',
    params
  })
}

/**
 * 班组列表（不分页）
 */
export function productionTeam(params) {
  return request({
    url: '/api/mes/enclosure/team',
    method: 'get',
    params
  })
}

/**
 * 质检班组列表（不分页）
 */
export function inspectionTeam(params) {
  return request({
    url: '/api/mes/enclosure/inspectionTeam',
    method: 'get',
    params
  })
}

/**
 * 保存生产组人员
 * @param {number} id|required id
 * @param {array} teamIds|required 班组ids
 */
export function saveProductionTeam(data) {
  return request({
    url: '/api/mes/enclosure/productionLine/team',
    method: 'post',
    data
  })
}

/**
 * 保存质检组人员
 * @param {number} id|required id
 * @param {array} teamIds|required 班组ids
 */
export function saveInspectionTeam(data) {
  return request({
    url: '/api/mes/enclosure/productionLine/inspection_team',
    method: 'post',
    data
  })
}

export default { get }
