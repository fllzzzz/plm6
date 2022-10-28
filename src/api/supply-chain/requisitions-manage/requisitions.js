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
 * @param {Boolean} boolPartyA 是否为甲供（项目备料类型）
 * @returns
 */
export function getSerialNumber(params) {
  return request({
    module: 'scm',
    url: `apply-purchase/get-serial-number`,
    method: 'get',
    params
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

// 撤销
export function del(ids) {
  return request({
    module: 'scm',
    url: `apply-purchase`,
    method: 'put',
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

export default { add, del, detail, get }
