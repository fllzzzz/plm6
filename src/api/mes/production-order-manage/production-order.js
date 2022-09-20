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

// 获取零构件清单明细
export function artifactTreeData(params) {
  return request({
    module: 'mes',
    url: 'scheduling/area/project',
    method: 'get',
    params
  })
}

// 获取构件清单明细
export function artifactDetail(params) {
  return request({
    module: 'mes',
    url: 'scheduling/area/artifact/list',
    method: 'get',
    params
  })
}

// 获取部件清单明细
export function assembleDetail(params) {
  return request({
    module: 'mes',
    url: 'scheduling/area/assemble/list',
    method: 'get',
    params
  })
}

// 获取零件清单明细
export function machinePartDetail(params) {
  return request({
    module: 'mes',
    url: 'scheduling/area/machinePart/list',
    method: 'get',
    params
  })
}

// 获取构零件明细汇总
export function artifactTreeSummary(params) {
  return request({
    module: 'mes',
    url: 'scheduling/area/summary',
    method: 'get',
    params
  })
}

// 获取生产订单汇总
export function productionOrderSummary(params) {
  return request({
    module: 'mes',
    url: 'scheduling/area/project/summay',
    method: 'get',
    params
  })
}

export default { get }
