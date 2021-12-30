import request from '@/utils/request'

export function get(params) {
  return request({
    module: 'contract',
    url: 'branchCompany',
    method: 'get',
    params
  })
}

export function add(data) {
  return request({
    module: 'contract',
    url: 'branchCompany',
    method: 'post',
    data
  })
}

export function edit(data) {
  return request({
    module: 'contract',
    url: 'branchCompany',
    method: 'put',
    data
  })
}

export function del(ids) {
  return request({
    module: 'contract',
    url: 'branchCompany',
    method: 'delete',
    data: ids
  })
}

export function editStatus(params) {
  return request({
    module: 'contract',
    url: 'branchCompany/enabled',
    method: 'put',
    params
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
