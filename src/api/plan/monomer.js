import request from '@/utils/request'

export function get(params) {
  return request({
    module: 'plan',
    url: 'monomer',
    method: 'get',
    params
  })
}

export function add(data) {
  return request({
    module: 'plan',
    url: 'monomer',
    method: 'post',
    data
  })
}

export function edit(data) {
  return request({
    module: 'plan',
    url: 'monomer',
    method: 'put',
    data
  })
}

export function del(ids) {
  return request({
    module: 'plan',
    url: 'monomer',
    method: 'delete',
    data: ids
  })
}

//获取所有单体信息 及对应已建区域信息
export function monomerAll(projectId) {
  return request({
    module: 'plan',
    url: `monomer/listByProjectId/${projectId}`,
    method: 'get'
  })
}

//获取单体信息的产品类型及完成时间
export function monomerDetail(id) {
  return request({
    module: 'plan',
    url: `monomer/listProductTypeListById/${id}`,
    method: 'get'
  })
}

export default { add, edit, del, get }
