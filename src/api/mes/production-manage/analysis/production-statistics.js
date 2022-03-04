import request from '@/utils/request'

/**
 *
 * 获取在制品统计汇总
 */
export function get(params) {
  return request({
    module: 'mes',
    url: 'analysis/production_summary',
    method: 'get',
    params
  })
}

/**
 *
 * 分组获取在制品统计汇总
 */
export function getByGroup(params) {
  return request({
    module: 'mes',
    url: 'analysis/production_summary/group',
    method: 'get',
    params
  })
}

/**
 *
 * 获取在制品统计详情-完成品
 */
export function getCompleteDetail(params) {
  return request({
    module: 'mes',
    url: 'analysis/production_summary/complete/details',
    method: 'get',
    params
  })
}

/**
 *
 * 获取在制品统计详情-在制品
 */
export function getInProductionDetail(params) {
  return request({
    module: 'mes',
    url: 'analysis/production_summary/in_production/details',
    method: 'get',
    params
  })
}

/**
 *
 * 获取在制品统计详情-未生产
 */
export function getUnProductionDetail(params) {
  return request({
    module: 'mes',
    url: 'analysis/production_summary/un_production/details',
    method: 'get',
    params
  })
}

export default {
  get
}
