import request from '@/utils/request'

/**
 * 甲供材料借出列表
 *
 * @param {Array} createTime  创建时间
 * @param {Number} page 页码
 * @param {Number} size 每页数量
 * @returns
 */
export function get(params) {
  return request({
    module: 'wms',
    url: 'transfer/party-a/borrow',
    method: 'get',
    params
  })
}

/**
 * 归还物料
 * @param {object} data
 * @returns
 */
export function returnMaterial(data) {
  return request({
    module: 'wms',
    url: 'transfer/party-a/borrow/return',
    method: 'put',
    data
  })
}

/**
 * 根据借用id获取可归还的物料列表
 * 根据配置获取
 * 在配置可从其他项目归还的情况下，能查询到其他项目的材料（这其中不包含“甲供”材料以及被借用项目的材料）
 * 未配置从其他项目归还的情况下，只查询当前项目库（可查询出“甲供”材料）和公共库
 * @param {*} id
 * @returns
 */
export function getReturnableMatListById(id) {
  return request({
    module: 'wms',
    url: `transfer/party-a/borrow/${id}/return-mat-list`,
    method: 'get'
  })
}

export default {
  get
}
