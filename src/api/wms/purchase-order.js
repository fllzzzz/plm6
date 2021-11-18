import request from '@/utils/request'

/**
 *订单列表
 *
 * @param {String} orderNo 订单号/合同号
 * @param {String} startTime 起始时间
 * @param {String} endTime 结束时间
 * @param {String} supplierName 供应商名称
 * @param {Number} type 订单类型
 * @param {Number} page 页码
 * @param {Number} size 每页数量
 * @returns
 */
export function get(params) {
  return request({
    module: 'wms',
    url: 'purchase-order',
    method: 'get',
    params
  })
}

/**
 *新增订单
 *
 * @export
 * @param {*} supplyType 供货类型【enum】
 * @param {*} orderNo 采购订单号
 * @param {*} organizeTime 编制日期
 * @param {*} basicClass 物料基础分类【enum】
 * @param {Number} projectId 项目id
 * @param {Array} purchaseIds 备料单号
 * @param {*} supplierId 供应商id
 * @param {*} supplierName 供应商名称
 * @param {Number} totalMete 合同量
 * @param {String} meteUnit 单位
 * @param {Number} contractAmount 合同额
 * @param {Number} invoiceType 票据类型【enum】
 * @param {Number} taxRate 税率
 * @param {Number} measurementType 计量方式【enum】
 * @param {Number} transportType 提货方式【enum】
 * @param {Number} type 订单类型【enum】
 */
export function add(data) {
  return request({
    module: 'wms',
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
    module: 'wms',
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
    module: 'wms',
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
    module: 'wms',
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
    module: 'wms',
    headers: { 'Content-Type': 'application/x-www-form-urlencoded' },
    url: `purchase-order/purchase-status`,
    method: 'put',
    data
  })
}

/**
 * 导出采购订单
 * @param {*} id  采购订单id
 */
export function download(id) {
  return request({
    module: 'wms',
    url: `purchase-order/export`,
    method: 'get',
    // responseType: 'blob',
    params: {
      id
    }
  })
}

export default { get, add, edit, del, download }
