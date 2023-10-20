import request from '@/utils/request'

/**
 * 获取结构商务列表
 * @param {number} projectId | required 项目id
 * @param {number} monomerId | required 单体id
 */
export function get(params) {
  return request({
    module: 'contract',
    url: 'business/artifact',
    method: 'get',
    params
  })
}

/**
 * 获取结构商务汇总
 * @param {number} projectId | required 项目id
 * @param {number} monomerId | required 单体id
 */
export function cost({ projectId, monomerId }) {
  return request({
    module: 'contract',
    url: 'business/artifact/summary',
    method: 'get',
    params: { projectId, monomerId }
  })
}

/**
 * 商务详情
 * @param {*} businessId 必填，商务构件id
 */
export function detail(businessId) {
  return request({
    module: 'contract',
    url: `business/artifact/${businessId}`,
    method: 'get'
  })
}

/**
 * 商务绑定
 * @param {Array} serialNumbers 必填，构件id
 * @param {Number} projectId 必填，项目id
 * @param {Number} businessId 必填，商务id
 */
export function businessBind(data) {
  return request({
    module: 'contract',
    url: 'business/bind',
    method: 'put',
    data
  })
}

/**
 * 构件商务下拉框
 * @param {*} monomerId 单体id
 */
export function businessList(params) {
  return request({
    module: 'contract',
    url: `business/artifact/simple`,
    method: 'get',
    params
  })
}

/**
 * 构件计价详情下载
 * @param {*} monomerId 单体id
 */
export function structureDetailDownload(params) {
  return request({
    url: `/api/business/artifact/download/${params.businessId}`,
    responseType: 'blob',
    method: 'get'
  })
}

export default { get, detail }
