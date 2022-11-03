import request from '@/utils/request'

/**
 *
 * 项目总览：获取获取当年的项目
 */
export function get(params) {
  return request({
    module: 'mes',
    url: 'kanban/project/year',
    method: 'get',
    params
  })
}
/**
 *
 * 项目总览：获取获取当年的项目下的工序清单
 */
export function getProcessList(params) {
  return request({
    module: 'mes',
    url: 'kanban/project/process/summary',
    method: 'get',
    params
  })
}
/**
 *
 * 项目总览：获取获取当年的项目下的某个工序的生产明细
 */
export function getProcessDetail(params) {
  return request({
    module: 'mes',
    url: 'kanban/project/process/summary/detail',
    method: 'get',
    params
  })
}
/**
 *
 * 项目总览：获取班组任务详情
 */
export function getTeamDetail(params) {
  return request({
    module: 'mes',
    url: 'kanban/project/group/task/detail',
    method: 'get',
    params
  })
}

export default { get }

