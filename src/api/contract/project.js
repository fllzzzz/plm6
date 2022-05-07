import request from '@/utils/request'

// 查询项目信息（项目按年份区别）
export function getProjectGroupByYear(params) {
  return request({
    module: 'contract',
    url: 'project/simple',
    method: 'get',
    params
  })
}

// 查询项目信息（用户/项目按年份区别）
export function getUserProjects(params) {
  return request({
    module: 'contract',
    url: 'user/project/simple',
    method: 'get',
    params
  })
}

// 查询可签证项目信息（签证用）
export function getUserVisaProjects(params) {
  return request({
    module: 'contract',
    url: 'user/project/visa/simple',
    method: 'get',
    params
  })
}

// 获取项目内容
export function getContentInfo(params) {
  return request({
    module: 'contract',
    url: `project/getAllType`,
    method: 'get',
    params
  })
}

export function get(params) {
  return request({
    module: 'contract',
    url: 'project/listAllProject',
    method: 'get',
    params
  })
}

export function add(data) {
  return request({
    module: 'contract',
    url: 'project',
    method: 'post',
    data
  })
}

export function editContract(data) {
  return request({
    module: 'contract',
    url: 'project/updateProject',
    method: 'post',
    data
  })
}

// 审核
export function confirmContract(data) {
  return request({
    module: 'contract',
    url: 'project/changeAudit',
    method: 'put',
    data
  })
}

export function del(ids) {
  return request({
    module: 'contract',
    url: 'project',
    method: 'delete',
    data: ids
  })
}

export function editStatus(projectId, status, data) {
  return request({
    module: 'contract',
    url: `project/${projectId}/status/${status}?isCheckOutFinishStatus=${data}`,
    method: 'put'
  })
}

export function editUsers(projectId, data) {
  return request({
    module: 'contract',
    url: `project/${projectId}/users`,
    method: 'put',
    data
  })
}

/**
 * 获取围护配置信息列表
 */
export function getEnclosureDictList(type) {
  return request({
    url: '/api/project/listByType/' + type,
    method: 'get'
  })
}

// 获取项目所有用户
export function getUserAllSimpleByProject(id) {
  return request({
    module: 'contract',
    url: `user/project/${id}/all/simple`,
    method: 'get'
  })
}

// 获取项目基本信息
export function getContractBase(projectId) {
  return request({
    module: 'contract',
    url: `project/${projectId}/base`,
    method: 'get'
  })
}

// 获取项目商务信息
export function getContractBusiness(projectId) {
  return request({
    module: 'contract',
    url: `project/${projectId}/business`,
    method: 'get'
  })
}

// 获取项目客户信息
export function getContractCustomer(projectId) {
  return request({
    module: 'contract',
    url: `project/${projectId}/customer`,
    method: 'get'
  })
}

// 获取项目技术交底
export function getContractTechInfo(projectId) {
  return request({
    module: 'contract',
    url: `project/techDisclosure/${projectId}`,
    method: 'get'
  })
}

// 获取所有分公司
export function getBranchCompanyAllSimple() {
  return request({
    module: 'contract',
    url: 'branchCompany/all/simple',
    method: 'get'
  })
}

/**
 * 下载合同基础附件
 * @param {*} id 文件id
 */
export function downloadBaseAttachments({ id }) {
  return request({
    module: 'contract',
    url: `common/attachment/download/${id}`,
    method: 'get',
    timeout: 6000000,
    responseType: 'blob'
  })
}

// 获取可完工数量
export function completeData() {
  return request({
    module: 'contract',
    url: 'project/getOutFinishCount',
    method: 'get'
  })
}

/**
 * 下载合同详情
 */
export function downloadProjectInfo({ projectId }) {
  return request({
    module: 'contract',
    url: `project/${projectId}/export`,
    method: 'get',
    timeout: 6000000,
    responseType: 'blob'
  })
}

// 项目数量统计
export function projectNumData() {
  return request({
    module: 'contract',
    url: 'project/project-Statistics',
    method: 'get'
  })
}
export default { get, add, del }
