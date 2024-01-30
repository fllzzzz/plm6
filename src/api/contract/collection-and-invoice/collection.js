import request from '@/utils/request'

export function get(params) {
  return request({
    module: 'contract',
    url: 'contract/collection/project',
    method: 'get',
    params
  })
}

export function getCollectionList(params) {
  return request({
    module: 'contract',
    url: 'contract/collection/listPage',
    method: 'get',
    params
  })
}

// export function add(data) {
//   return request({
//     module: 'contract',
//     url: 'contract/collection/save',
//     method: 'post',
//     data
//   })
// }

export function add(data) {
  return request({
    module: 'contract',
    url: 'contract/collection/project',
    method: 'post',
    params: { projectId: data.projectId },
    data: data.list
  })
}

export function edit(data) {
  return request({
    module: 'contract',
    url: 'contract/collection/project',
    method: 'put',
    data
  })
}

export function del(id) {
  return request({
    module: 'contract',
    url: `contract/collection/deleteById/${id}/project`,
    method: 'delete'
  })
}

export function contractCollectionInfo(params) {
  return request({
    module: 'contract',
    url: 'contract/collection/getFinanceInfo',
    method: 'get',
    params,
    cancelKey: false
  })
}

export function editStatus(data) {
  return request({
    module: 'contract',
    url: `contract/collection/audit/project`,
    method: 'put',
    data
  })
}

export function bankData(companyId) {
  return request({
    module: 'contract',
    url: `branchCompany/listBankAccountByCompanyId/${companyId}`,
    method: 'get'
  })
}

export function getTotalNumber(params) {
  return request({
    module: 'contract',
    url: 'contract/collection/getCollectionSum',
    method: 'get',
    params
  })
}

export default { get, add, edit, del }
