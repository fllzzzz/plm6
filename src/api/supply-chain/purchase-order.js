import request from '@/utils/request'

/**
 * 采购合同-采购中（简要的）
 * @param {*} params
 * @returns
 */
export function getPurchasingPurchaseOrderBrief(params) {
  return request({
    module: 'scm',
    url: 'purchase-order/purchasing/all/brief',
    method: 'get',
    params
  })
}

/**
 * 全部采购合同（含已完成的）
 * @param {*} params
 * @returns
 */
export function getPurchaseOrder(params) {
  return request({
    module: 'scm',
    url: 'purchase-order/purchasing/serialNumber',
    method: 'get',
    params
  })
}

/**
 * 订单列表
 *
 * @param {String} serialNumber 订单号/合同号
 * @param {Array} createTime  创建时间
 * @param {String} supplierName 供应商名称
 * @param {Number} type 订单类型
 * @param {Number} page 页码
 * @param {Number} size 每页数量
 * @returns
 */
export function get(params) {
  return request({
    module: 'scm',
    url: 'purchase-order',
    method: 'get',
    params
  })
}

/**
 * 新增订单
 * @param {*} data
 * @returns
 */
export function add(data) {
  return request({
    module: 'scm',
    url: 'purchase-order',
    method: 'post',
    data
  })
}

/**
 *修改订单
 *
 * @export
 * @returns
 */
export function edit(data) {
  return request({
    module: 'scm',
    url: 'purchase-order',
    method: 'put',
    data
  })
}

/**
 *删除订单
 *
 * @export
 * @param {Array} ids 要删除的id集合
 * @returns
 */
export function del(ids) {
  return request({
    module: 'scm',
    url: 'purchase-order',
    method: 'delete',
    data: ids
  })
}

/**
 * 订单详情
 * @param {*} id 详情id
 */
export function detail(id) {
  return request({
    module: 'scm',
    url: `purchase-order/${id}`,
    method: 'get'
  })
}

/**
 * 修改订单状态
 * @param {number} id | required 订单id
 * @param {number} status | required 订单状态
 */
export function editPurchaseStatus(data) {
  return request({
    module: 'scm',
    headers: { 'Content-Type': 'application/x-www-form-urlencoded' },
    url: `purchase-order/purchase-status`,
    method: 'put',
    data
  })
}

/**
 * 导出采购合同
 * @param {*} id  采购合同id
 */
export function download(id) {
  return request({
    module: 'scm',
    url: `purchase-order/export`,
    method: 'get',
    responseType: 'blob',
    params: {
      id
    }
  })
}

export default { get, add, edit, del, detail, download }
