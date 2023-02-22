
import request from '@/utils/request'

/**
 * @description: 生产监控看板
 */
export function get(params) {
  return request({
    module: 'mes',
    url: 'scheduling/artifact/product/monitor/kanban',
    method: 'get',
    params
  })
}

/**
 * @description: 生产监控看板汇总
 */
export function getSummary(params) {
  return request({
    module: 'mes',
    url: 'scheduling/artifact/product/monitor/kanban/summary',
    method: 'get',
    params
  })
}

/**
 * @description: 生产监控看板详情
 */
export function getDetail(params) {
  return request({
    module: 'mes',
    url: 'scheduling/artifact/product/monitor/kanban/detail',
    method: 'get',
    params
  })
}

/**
 * @description: 生产监控班组弹窗
 */
export function getGroupDialog(params) {
  return request({
    module: 'mes',
    url: 'scheduling/artifact/product/monitor/group',
    method: 'get',
    params
  })
}
/**
 * @description: 生产监控班组详情
 */
export function getGroupDetail(params) {
  return request({
    module: 'mes',
    url: 'scheduling/artifact/product/monitor/group/detail',
    method: 'get',
    params
  })
}

export default { get }

