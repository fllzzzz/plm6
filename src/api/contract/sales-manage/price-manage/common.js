import request from '@/utils/request'

/**
 * 保存后商务价格
 * @param {Number} type | required 产品类型
 * @param {Number} monomerId | required 单体id
 * @param {String} remark  备注
 * @param {Array} details  修改详情【{id， unitPrice}】
 */
export function save(data) {
  return request({
    module: 'contract',
    url: 'business',
    method: 'put',
    data
  })
}

/**
 * 项目及单体造价
 * @param {*} projectId 必填，项目id
 * @param {*} monomerId 选填，单体id
 */
export function cost({ projectId, monomerId, areaId, type }) {
  return request({
    module: 'contract',
    url: 'business/summary',
    method: 'get',
    params: { projectId, monomerId, areaId, type }
  })
}

/**
 * 商务价格审核列表
 * @param {Number} projectId 项目id
 * @param {Number} productType 产品类型
 * @param {Number} status 审核状态
 */
export function priceModifyGet(params) {
  return request({
    module: 'contract',
    url: 'business/check',
    method: 'get',
    params
  })
}

/**
 * 商务价格审核
 * @param {Number} id 商务审核清单id
 */
export function priceModifySave({ id, status }) {
  return request({
    module: 'contract',
    url: 'business/check',
    method: 'put',
    data: { id, status }
  })
}

/**
 * 获取商务待未审核数量
 * @param {Number} projectId 项目id
 */
export function priceModifyCount() {
  return request({
    module: 'contract',
    url: 'business/check/count',
    method: 'get'
  })
}

/**
 * 商务价格审核详情列表
 * @param {Number} id 商务审核清单id
 */
export function priceModifyDetail(id) {
  return request({
    module: 'contract',
    url: `business/check/${id}`,
    method: 'get'
  })
}

// 价格提交记录列表
export function getTempPrice(params) {
  return request({
    module: 'contract',
    url: 'business/tempPrice',
    method: 'get',
    params
  })
}

// 删除价格提交记录
export function deleteTempPrice(ids) {
  return request({
    module: 'contract',
    url: 'business/tempPrice',
    method: 'delete',
    data: ids
  })
}

// 记录提交审核
export function saveTempPrice(data) {
  return request({
    module: 'contract',
    url: 'business/tempPrice/commitCheck',
    method: 'post',
    data
  })
}

// 获取待提交数量
export function saveNum(params) {
  return request({
    module: 'contract',
    url: 'business/tempPrice/count',
    method: 'get',
    params
  })
}
