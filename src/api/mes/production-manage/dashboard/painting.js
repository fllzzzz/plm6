import request from '@/utils/request'

/**
 *
 * 获取涂装列表
 */
export function get(params) {
  return request({
    module: 'mes',
    url: 'kanban/painting/setting/list',
    method: 'get',
    params
  })
}

/**
 *
 * 获取单体下各个类型的构件油漆消耗
 */
export function getAllArtifact(params) {
  return request({
    module: 'mes',
    url: 'kanban/painting/type',
    method: 'get',
    params
  })
}

/**
 *
 * 获取构件
 */
export function artifactList(params) {
  return request({
    module: 'mes',
    url: 'kanban/painting/artifact/page',
    method: 'get',
    params
  })
}

/**
 *
 * 获取构件汇总
 */
export function artifactSummary(params) {
  return request({
    module: 'mes',
    url: 'kanban/painting/artifact/page/summary',
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
    module: 'mes',
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
    module: 'mes',
    url: 'kanban/painting/artifact',
    method: 'post',
    data
  })
}

export default { get }
