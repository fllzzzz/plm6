import request from '@/utils/request'

/**
 * 生产订单排期列表
 */
export function get(params) {
  return request({
    module: 'bridge',
    url: 'scheduling/area/project/page',
    method: 'get',
    params
  })
}

// 获取项目生产订单排期计划
export function scheduleDetail(params) {
  return request({
    module: 'bridge',
    url: 'scheduling/area/list',
    method: 'get',
    params
  })
}

export function updateSchedule(data) {
  return request({
    module: 'bridge',
    url: 'scheduling/area',
    method: 'put',
    data
  })
}

// 获取分段清单明细
export function boxTreeData(params) {
  return request({
    module: 'bridge',
    url: 'scheduling/area/project',
    method: 'get',
    params
  })
}

// 获取分段清单明细
export function boxDetail(params) {
  return request({
    module: 'bridge',
    url: 'scheduling/area/box/list',
    method: 'get',
    params
  })
}

// 获取单元件清单明细
export function elementDetail(params) {
  return request({
    module: 'bridge',
    url: 'scheduling/area/element/list',
    method: 'get',
    params
  })
}

// 获取零件清单明细
export function machinePartDetail(params) {
  return request({
    module: 'bridge',
    url: 'scheduling/area/machinePart/list',
    method: 'get',
    params
  })
}

// 获取分段零件明细汇总
export function boxTreeSummary(params) {
  return request({
    module: 'bridge',
    url: 'scheduling/area/summary',
    method: 'get',
    params
  })
}

// 获取生产订单汇总
export function productionOrderSummary(params) {
  return request({
    module: 'bridge',
    url: 'scheduling/area/project/summay',
    method: 'get',
    params
  })
}

export default { get }
