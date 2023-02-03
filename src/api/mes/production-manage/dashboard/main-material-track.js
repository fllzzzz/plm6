import request from '@/utils/request'

/**
 *
 * 获取主材跟踪汇总
 * @export
 * @param {*} projectId|required 页码
 * @returns
 */
export function getSummary(params) {
  return request({
    module: 'mes',
    url: 'kanban/tracking/summary',
    method: 'get',
    params
  })
}

/**
 *
 * 获取主材跟踪对比量列表
 * @export
 * @param {*} projectId|required 页码
 * @returns
 */
export function getCompare(params) {
  return request({
    module: 'mes',
    url: 'kanban/tracking',
    method: 'get',
    params
  })
}

/**
 *
 * 获取构件生产记录
 * @export
 * @param {*} projectId|required 页码
 * @returns
 */
export function productionRecord(params) {
  return request({
    module: 'mes',
    url: 'kanban/tracking/artifact/record',
    method: 'get',
    params
  })
}

/**
 *
 * 获取构件生产记录
 * @export
 * @param {*} projectId|required 页码
 * @returns
 */
export function productionDetail(params) {
  return request({
    module: 'mes',
    url: 'kanban/tracking/artifact/record/detail',
    method: 'get',
    params
  })
}

/**
 *
 * 获取钢材出库记录
 * @export
 * @param {*} projectId|required 页码
 * @returns
 */
export function outboundRecord(params) {
  return request({
    module: 'mes',
    url: 'kanban/tracking/plate/record',
    method: 'get',
    params
  })
}

/**
 *
 * 获取钢材出库记录
 * @export
 * @param {*} projectId|required 页码
 * @returns
 */
export function outboundDetail(params) {
  return request({
    module: 'mes',
    url: 'kanban/tracking/plate/record/detail',
    method: 'get',
    params
  })
}

/**
 *
 * 钢材库领用详情
 */
export function useDetail(params) {
  return request({
    module: 'mes',
    url: 'kanban/tracking/use',
    method: 'get',
    params
  })
}

/**
 *
 * 钢材库存量详情
 */
export function stockDetail(params) {
  return request({
    module: 'mes',
    url: 'kanban/tracking/inventory',
    method: 'get',
    params
  })
}
