import request from '@/utils/request'

/**
 *
 * 获取qhse列表
 * @export
 * @param {*} page|required 页码
 * @param {*} size|required 页大小
 * @returns
 */
export function get(params) {
  return request({
    module: 'mes',
    url: 'qhse/page',
    method: 'get',
    params
  })
}

/**
 *
 * 导入问题报告excel
 * @export
 * @param {*} page|required 页码
 * @param {*} size|required 页大小
 * @returns
 */
export function getExcelFn({ id }) {
  return request({
    module: 'mes',
    url: `qhse/detail/${id}/download`,
    responseType: 'blob',
    method: 'get'
  })
}

export default {
  get
}
