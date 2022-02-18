import request from '@/utils/request'

/**
 * 获取对外签证列表
 * @param {number} page|required 页码
 * @param {number} size|required 页大小
 * @param {number} projectId 项目id
 * @param {string} userName 申请人名称
 * @param {string} checkUerName 审核人名称
 * @returns
 */
export function get(params) {
  return request({
    module: 'contract',
    url: 'visa/foreign',
    method: 'get',
    params
  })
}

/**
 * 新增签证单
 * @param {number} reasonId|required 签证原因（字典）
 * @param {number} projectId|required 项目id
 * @param {number} userId|required 签证人
 * @param {number} visaAmount|required 签证额
 * @param {number} visaDate|required 签证日期
 * @param {string} supervisionUnit 监理单位
 * @param {string} designUnit 设计单位
 * @param {string} remark 备注
 * @returns
 */
export function addVisa(data) {
  return request({
    module: 'contract',
    url: 'visa/foreign',
    method: 'post',
    data
  })
}

/**
 * 编辑签证单
 * @param {number} id|required 签证单id
 * @param {number} reasonId|required 签证原因（字典）
 * @param {number} projectId|required 项目id
 * @param {number} userId|required 签证人
 * @param {number} visaAmount|required 签证额
 * @param {number} visaDate|required 签证日期
 * @param {string} supervisionUnit 监理单位
 * @param {string} designUnit 设计单位
 * @param {string} remark 备注
 * @returns
 */
export function editVisa(data) {
  return request({
    module: 'contract',
    url: 'visa/foreign',
    method: 'put',
    data
  })
}

/**
 * 新增结算单
 * @param {number} projectId|required 项目id
 * @param {number} breachAmount|required 违约额
 * @param {number} settlementAmount|required 结算额
 * @param {number} visaAmount|required 签证额
 * @param {string} remark 备注
 * @returns
 */
export function addSettlement(data) {
  return request({
    module: 'contract',
    url: 'visa/project/handle',
    method: 'post',
    data
  })
}

/**
 * 编辑结算单
 * @param {number} id|required 签证单id
 * @param {number} projectId|required 项目id
 * @param {number} breachAmount|required 违约额
 * @param {number} settlementAmount|required 结算额
 * @param {number} visaAmount|required 签证额
 * @param {string} remark 备注
 * @returns
 */
export function editSettlement(data) {
  return request({
    module: 'contract',
    url: 'visa/project/handle',
    method: 'put',
    data
  })
}

/**
 * 获取项目信息
 * @param {number} projectId 项目id
 * @returns
 */
export function getProjectInfo(projectId) {
  return request({
    module: 'contract',
    url: `visa/project/${projectId}`,
    method: 'get'
  })
}

/**
 * 确签（审核）
 */
export function check(data) {
  return request({
    module: 'contract',
    url: 'visa',
    method: 'post',
    data
  })
}

/**
 * 签证详情
 */
export function detail(id) {
  return request({
    module: 'contract',
    url: `visa/${id}`,
    method: 'post'
  })
}

/**
 * 下载签证单
 * @param {number} id|required 签证单id
 */
export function downloadVisa(id) {
  return request({
    module: 'contract',
    url: `visa/${id}/export`,
    method: 'get',
    responseType: 'blob'
  })
}

/**
 * 下载结算单
 * @param {number} id|required 签证单id
 */
export function downloadSettlement(id) {
  return request({
    module: 'contract',
    url: `visa/${id}/settlement/export`,
    method: 'get',
    responseType: 'blob'
  })
}

export default { get, detail }
