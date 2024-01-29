import request from '@/utils/request'

export function get(params) {
  return request({
    module: 'contract',
    url: 'contract/collection/waste',
    method: 'get',
    params
  })
}

export function add(data) {
  return request({
    module: 'contract',
    url: 'contract/collection/waste',
    method: 'post',
    params: { projectId: data.projectId },
    data: data.list
  })
}

export function edit(data) {
  return request({
    module: 'contract',
    url: 'contract/collection/waste',
    method: 'put',
    data
  })
}

export function del(id) {
  return request({
    module: 'contract',
    url: `contract/collection/deleteById/${id}/waste`,
    method: 'delete'
  })
}

export function editStatus(data) {
  return request({
    module: 'contract',
    url: `contract/collection/audit/waste`,
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

export default { get, add, edit, del }
