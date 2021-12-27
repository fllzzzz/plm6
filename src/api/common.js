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
    params: { hasRoot }
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
    module: 'wms',
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
