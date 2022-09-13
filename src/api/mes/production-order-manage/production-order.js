import request from '@/utils/request'

/**
 * 生产订单排期列表
 */
export function get(params) {
  return request({
    module: 'mes',
    url: 'scheduling/area/project/page',
    method: 'get',
    params
  })
}

// 获取项目生产订单排期计划
export function scheduleDetail(params) {
  return request({
    module: 'mes',
    url: 'scheduling/area/list',
    method: 'get',
    params
  })
}

export function updateSchedule(data) {
  return request({
    module: 'mes',
    url: 'scheduling/area',
    method: 'put',
    data
  })
}

export default { get }
