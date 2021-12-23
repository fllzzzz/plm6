import request from '@/utils/request'

export function get(params) {
  return request({
    module: 'contract',
    url: 'project/listChange',
    method: 'get',
    params
  })
}

export function editStatus(params) {
  return request({
    module: 'contract',
    url: 'project/changeAudit',
    method: 'get',
    params
  })
}

export default { get }
