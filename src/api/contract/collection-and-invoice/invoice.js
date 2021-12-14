import request from '@/utils/request'

export function get(params) {
  return request({
    module: 'contract',
    url: 'contract/invoice/listPage',
    method: 'get',
    params
  })
}

export function add(data) {
  return request({
    module: 'contract',
    url: 'contract/invoice/save',
    method: 'post',
    data
  })
}


//审核
export function editStatus(id,status) {
  return request({
    module: 'contract',
    url: `contract/invoice/audit/invoiceId/${id}/status/${status}`,
    method: 'put'
  })
}

export default { get, add }