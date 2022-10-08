import request from '@/utils/request'

export function get(params) {
  return request({
    module: 'project',
    url: `install-plan/list`,
    method: 'get',
    params
  })
}

// 批量修改
export function edit(data) {
  return request({
    module: 'project',
    url: `install-plan/setSupplier`,
    method: 'post',
    data
  })
}

// 获取分包商登录账号列表
export function supplierAccountList(params) {
  return request({
    module: 'project',
    url: `install-plan/listSupplier`,
    method: 'get',
    params
  })
}

export default { edit, get }
