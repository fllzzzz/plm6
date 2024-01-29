import request from '@/utils/request'

/**
 *
 * 获取质检报表列表
 * @export
 * @returns
 */
export function get(params) {
  return request({
    module: 'mes',
    url: 'report/inspection/summary/page',
    method: 'get',
    params
  })
}

/**
 *
 * 获取质检报表列表详情
 * @export
 * @returns
 */
export function detail(params) {
  return request({
    module: 'mes',
    url: 'report/inspection/summary/details/page',
    method: 'get',
    params
  })
}

/**
 *
 * 获取质检详情工序列表
 * @export
 * @returns
 */
export function getProcess(params) {
  return request({
    module: 'mes',
    url: 'report/inspection/summary/details/getprocesslist',
    method: 'get',
    params
  })
}

export function downloadDetaile(params) {
  return request({
    module: 'mes',
    url: `report/inspection/summary/details/print`,
    method: 'get',
    responseType: 'blob',
    params
  })
}

export default {
  get
}
