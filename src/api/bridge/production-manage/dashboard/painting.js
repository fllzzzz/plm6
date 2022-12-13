import request from '@/utils/request'

/**
 *
 * 获取涂装列表
 */
export function get(params) {
  return request({
    module: 'bridge',
    url: 'kanban/painting/setting/list',
    method: 'get',
    params
  })
}

/**
 *
 * 获取单体下各个类型的分段油漆消耗
 */
export function getAllBox(params) {
  return request({
    module: 'bridge',
    url: 'kanban/painting/type',
    method: 'get',
    params
  })
}

/**
 *
 * 获取分段
 */
export function boxList(params) {
  return request({
    module: 'bridge',
    url: 'kanban/painting/box/page',
    method: 'get',
    params
  })
}

/**
 *
 * 获取分段汇总
 */
export function boxSummary(params) {
  return request({
    module: 'bridge',
    url: 'kanban/painting/box/page/summary',
    method: 'get',
    params
  })
}

/**
 *
 * 涂装修改
 */
export function change(data) {
  return request({
    module: 'bridge',
    url: 'kanban/painting',
    method: 'post',
    data
  })
}

/**
 *
 * 涂装面积修改
 */
export function editArea(data) {
  return request({
    module: 'bridge',
    url: 'kanban/painting/box',
    method: 'post',
    data
  })
}

export default { get }
