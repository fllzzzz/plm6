import request from '@/utils/request'

/**
 *
 * 获取项目报表列表
 */
export function getSummaryList(params) {
  return request({
    module: 'mes',
    url: 'kanban/form/formSummary',
    method: 'get',
    params
  })
}

/**
 *
 * 获取发运列表
 */
export function getShipList(params) {
  return request({
    module: 'mes',
    url: 'cargo/report/ship',
    method: 'get',
    params
  })
}

/**
 *
 * 获取qhse列表
 */
export function getQhseList(params) {
  return request({
    module: 'mes',
    url: 'qhse/summary',
    method: 'get',
    params
  })
}

