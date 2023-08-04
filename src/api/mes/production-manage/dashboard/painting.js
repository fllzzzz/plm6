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

/**
 * 获取油漆手工填报列表
 */
export function manualList(params) {
  return request({
    module: 'mes',
    url: `kanban/painting/manual/entry/list`,
    method: 'get',
    params
  })
}

/**
 * 保存或者更新油漆手工填报列表
 */
export function manualEdit(data) {
  return request({
    module: 'mes',
    url: `kanban/painting/save/manual/entry/list`,
    method: 'post',
    data
  })
}

/**
 * 下载油漆手工填报列表
 */
export function downloadManualList(params) {
  return request({
    module: 'mes',
    url: `kanban/painting/download/manual/entry/list`,
    responseType: 'blob',
    method: 'get',
    params
  })
}

export default { get }
