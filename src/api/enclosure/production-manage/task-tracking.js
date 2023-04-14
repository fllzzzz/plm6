import request from '@/utils/request'

/**
 * 获取围护项目
 * @export
 * @param {string} year 年份
 * @param {string} name 项目模糊查询
 * @param {number} state 排产状态 1: 生产中 2:部分排产 4:已完成
 * @returns
 */
export function projectList(params) {
  return request({
    module: 'enclosure',
    url: 'produce/project',
    method: 'get',
    params
  })
}

/**
 * 获取项目下的工单列表
 * @export
 * @param {*} name|required 页码
 * @param {*} size|required 页大小
 * @param {number} projectId|required 项目id
 * @param {string} userName 排产人
 * @param {string} orderNumber 工单号
 * @returns
 */
export function get(params) {
  return request({
    module: 'enclosure',
    url: 'produce/order/list',
    method: 'get',
    params
  })
}

/**
 * 获取工单详情
 * @export
 * @param {*} name|required 页码
 * @param {*} size|required 页大小
 * @param {number} id|required 工单id
 * @returns
 */
export function detail(id) {
  return request({
    module: 'enclosure',
    url: 'produce/order/detail',
    method: 'get',
    params: { id }
  })
}

export default { get, detail }
