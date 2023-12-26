import request from '@/utils/request'

/**
 * 获取转换单列表
 */
export function get(params) {
  return request({
    module: 'wms',
    url: 'convert/convert',
    method: 'get',
    params
  })
}

/**
 * 转换单详情
 */
export function detail(id) {
  return request({
    module: 'wms',
    url: `convert/convertDetail/${id}`,
    method: 'get',
    cancelKey: false
  })
}

/**
 * 转换单详情
 */
export function getDetail(id) {
  return request({
    module: 'wms',
    url: `convert/convertDetail/${id}`,
    method: 'get',
    cancelKey: false
  })
}

/**
 * 审核退回
 * @param {*} id 详情id
 */
export function reviewReturned(id) {
  return request({
    module: 'wms',
    url: `convert/convertReturn/${id}`,
    method: 'get'
  })
}

/**
 * 审核通过
 * @param {*} id 详情id
 */
export function reviewPassed(id) {
  return request({
    module: 'wms',
    url: `convert/passed/${id}`,
    method: 'get'
  })
}

/**
 * 获取待审核转换单数量
 * @param {*} id 详情id
 */
export function auditNum(id) {
  return request({
    module: 'wms',
    url: `convert/reviewedNum`,
    method: 'get'
  })
}

// 转换单报表
export function convertListReport(params) {
  return request({
    module: 'wms',
    url: `convert/getProjectConvert`,
    method: 'get',
    params
  })
}
export default { get, detail }
