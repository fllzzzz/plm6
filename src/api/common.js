import request from '@/utils/request'

// // 加载菜单
// export function fetchMenus() {
//   return request({
//     module: 'common',
//     url: 'menu/build',
//     method: 'get'
//   })
// }

// 获取所有用户
export function getUserAllSimple(params) {
  return request({
    url: 'api/user/all/simple',
    method: 'get',
    params,
    cancelKey: false
  })
}

// 省市区级联
export function getRegionalCascade() {
  return request({
    url: '/api/regionalCascade',
    method: 'get'
  })
}

// 获取所有部门
export function getDeptAllSimple(hasRoot = true) {
  return request({
    module: 'user',
    url: 'dept/all/simple',
    method: 'get',
    params: { hasRoot: true }
  })
}

// 获取部门用户
export function getDeptAllUser(params) {
  return request({
    module: 'user',
    url: 'user/dept',
    method: 'get',
    params
  })
}

// 获取所有用户,带部门
export function getUserTree() {
  return request({
    module: 'user',
    method: 'get',
    url: 'user/tree'
  })
}

// 获取所有供应商（简要的）
export function getSuppliersBrief() {
  return request({
    module: 'scm',
    url: `supplier/all/brief`,
    method: 'get'
  })
}

/**
 * 上传公用附件
 * @param {number} fileType 文件类型
 * @param {string} file 二进制文件
 */
export function uploadAttachment(data) {
  return request({
    module: 'common',
    url: 'attachment',
    method: 'post',
    headers: { 'Content-Type': 'multipart/form-data' },
    data
  })
}

// 附件公用下载
export function downloadAttachment({ id }) {
  return request({
    module: 'common',
    url: `attachment/download/${id}`,
    method: 'get',
    timeout: 6000000,
    responseType: 'blob'
  })
}

/**
 * 获取全部项目（多模块使用）
 * @export
 * @param {number} page|required 页码
 * @param {number} size|required 页大小
 * @param {number} year 年份
 * @param {string} noOrProjectName 合同编号或项目简称
 * @returns
 */
export function getProjectList(params) {
  return request({
    module: 'contract',
    url: 'project/listMy',
    method: 'get',
    params
  })
}

/**
 * 获取项目汇总信息（多模块使用）
 * @export
 * @param {number} year 年份
 * @returns
 */
export function getProjectInfo(params) {
  return request({
    module: 'contract',
    url: 'project/getMyProjectInfo',
    method: 'get',
    params
  })
}

/**
 * 获取围护生产组信息 车间-产线-生产组
 */
export function enclosureGroupsTree(params) {
  return request({
    module: 'enclosure',
    url: 'scheduling/tree',
    method: 'get',
    params
  })
}
