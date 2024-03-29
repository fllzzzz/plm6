import request from '@/utils/request'

/**
 * @description: 获取月度任务跟踪列表（按月查询）
 */
export function get(params) {
  return request({
    module: 'bridge',
    url: 'task/tracking/month',
    method: 'get',
    params
  })
}
/**
 * @description: 获取某个月项目
 */
export function monthlyProject(params) {
  return request({
    module: 'bridge',
    url: 'task/tracking/month/page',
    method: 'get',
    params
  })
}
/**
 * @description: 获取某个月某个项目下的任务跟踪列表
 */
export function projectDetail(params) {
  return request({
    module: 'bridge',
    url: 'task/tracking/month/project/page',
    method: 'get',
    params
  })
}
export default { get }
