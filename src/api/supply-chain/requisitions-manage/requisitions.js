import request from '@/utils/request'

/**
 * 材料申购列表
 * @param {String} serialNumber 申购编号
 * @param {Array} times  创建时间
 * @param {Number} status 订单审核状态
 * @param {Number} materialType 材料分类
 * @param {Number} page 页码
 * @param {Number} size 每页数量
 * @returns
 */
export function get(params) {
  return request({
    module: 'scm',
    url: `apply-purchase`,
    method: 'get',
    params
  })
}

/**
 * 材料申购详情
 * @param {Number} id 申购id
 * @returns
 */
export function detail(id) {
  return request({
    module: 'scm',
    url: `apply-purchase/${id}`,
    method: 'get'
  })
}

/**
 * 获取申购单号
 * @returns
 */
export function getSerialNumber() {
  return request({
    module: 'scm',
    url: `apply-purchase/get-serial-number`,
    method: 'get'
  })
}

// 新增
export function add(data) {
  return request({
    module: 'scm',
    url: `apply-purchase`,
    method: 'post',
    data
  })
}

// 新增
export function edit(data) {
  return request({
    module: 'scm',
    url: `apply-purchase`,
    method: 'put',
    data
  })
}

// 撤销
export function del(ids) {
  return request({
    module: 'scm',
    url: `apply-purchase`,
    method: 'delete',
    params: {
      id: ids[0]
    }
  })
}

// 编辑申购单状态
export function editStatus({ id, enabled }) {
  return request({
    module: 'scm',
    url: `apply-purchase/enabled`,
    method: 'put',
    params: { id, enabled }
  })
}

// 申购单（简要）
export function getRequisitionBrief(params) {
  return request({
    module: 'scm',
    url: `apply-purchase/brief`,
    method: 'get',
    params
  })
}

// 查询可采购物料详情
export function canPurchaseDetail(params) {
  return request({
    module: 'scm',
    url: `apply-purchase/can-purchase-detail`,
    method: 'get',
    params
  })
}

/**
 * 查询构件类型
 *
 * @param {Number} page 页码
 * @param {Number} size 每页数量
 * @returns
 */
export function manufClassListGet(params) {
  return request({
    module: 'scm',
    url: 'purchase-order/finished-product/purchasing/structure-class',
    method: 'get',
    params
  })
}

/**
 * 成品列表
 *
 * @param {Number} page 页码
 * @param {Number} size 每页数量
 * @returns
 */
export function manufListGet(params) {
  return request({
    module: 'scm',
    url: 'purchase-order/finished-product/purchasing/artifact-enclosure-detail',
    method: 'get',
    params
  })
}

/**
 * 可申购库存查询
 *
 * @returns
 */
export function inventoryGet(params) {
  return request({
    module: 'wms',
    url: 'material-inventory/apply-purchase-steel',
    method: 'get',
    params
  })
}

export default { add, edit, del, detail, get }
