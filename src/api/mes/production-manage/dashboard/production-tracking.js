
import request from '@/utils/request'

/**
 *
 * 生产跟踪/构件
 */
export function get(params) {
  return request({
    module: 'mes',
    url: 'task/process/artifact/process/track',
    method: 'get',
    params
  })
}

/**
 *
 * 生产跟踪/部件
 */
export function assembleTrack(params) {
  return request({
    module: 'mes',
    url: 'task/process/assemble/process/track',
    method: 'get',
    params
  })
}

/**
 *
 * 生产跟踪/ 构、部件工序类型汇总
 */
export function artifactAssembleList(params) {
  return request({
    module: 'mes',
    url: 'task/process/type/list',
    method: 'get',
    params
  })
}

/**
 *
 * 生产线筛选列表
 */
export function getLines(params) {
  return request({
    module: 'mes',
    url: 'task/process/assemble/process/track/prodLineList',
    method: 'get',
    params
  })
}

/**
 *
 * 下载生产跟踪excel表格
 */
export function downloadFn(params) {
  return request({
    module: 'mes',
    url: 'task/process/assemble/process/track/print',
    method: 'get',
    responseType: 'blob',
    params
  })
}

export default { get }

