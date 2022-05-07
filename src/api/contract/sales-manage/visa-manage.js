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
export function add(data) {
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
export function edit(data) {
  return request({
    module: 'contract',
    url: 'visa/foreign',
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
export function download(id) {
  return request({
    module: 'contract',
    url: `visa/${id}/export`,
    method: 'get',
    responseType: 'blob'
  })
}

export default { get, add, edit, detail, check, download }
