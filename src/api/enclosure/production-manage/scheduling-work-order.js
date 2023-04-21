import request from '@/utils/request'

/**
 * 获取围护项目
 * @export
 * @param {string} year 年份
 * @param {string} name 项目模糊查询
 * @returns
 */
export function projectList(params) {
  return request({
    module: 'enclosure',
    url: 'order/project/list',
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
    url: 'order/order/list',
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
    url: 'order/order/detail',
    method: 'get',
    params: { id }
  })
}

/**
 * 删除工单
 */
export function del(ids) {
  return request({
    module: 'enclosure',
    url: 'order/order',
    method: 'delete',
    data: ids
  })
}

/**
 * 打印成功后上报
 * @export
 * @param {number} id|required 工单id
 * @returns
 */
export function report(id) {
  return request({
    module: 'enclosure',
    url: 'order/order/detail/save/print',
    method: 'put',
    params: { id }
  })
}

export default { get, del, detail }
