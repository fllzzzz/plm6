import request from '@/utils/request'

/**
 * 获取对外签证列表
 * @param {number} page|required 页码
 * @param {number} size|required 页大小
 * @param {number} projectId 项目id
 * @param {number} businessType 签证类型[businessTypeEnum]
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
 * 新增结算单
 * @param {number} projectId|required 项目id
 * @param {number} breachAmount|required 违约额
 * @param {number} settlementAmount|required 结算额
 * @param {number} visaAmount|required 签证额
 * @param {string} remark 备注
 * @returns
 */
export function add(data) {
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
export function edit(data) {
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
 * 下载结算单
 * @param {number} id|required 签证单id
 */
export function download(id) {
  return request({
    module: 'contract',
    url: `visa/${id}/settlement/export`,
    method: 'get',
    responseType: 'blob'
  })
}

export default { get, add, edit, detail, check, download }
