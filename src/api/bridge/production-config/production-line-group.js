import request from '@/utils/request'

export function get(params) {
  return request({
    module: 'bridge',
    url: 'groups/page',
    method: 'get',
    params
  })
}

export function add(data) {
  return request({
    module: 'bridge',
    url: 'groups',
    method: 'post',
    data
  })
}

export function edit(data) {
  return request({
    module: 'bridge',
    url: 'groups',
    method: 'put',
    data
  })
}

export function del(ids) {
  return request({
    module: 'bridge',
    url: 'groups',
    method: 'delete',
    data: { ids }
  })
}

// 生产组下批量绑定班组
export function productAddTeam({ productLineId, groupId, teamIds }) {
  return request({
    module: 'bridge',
    url: 'groups/team',
    method: 'post',
    data: { id: groupId, teamIds }
  })
}

// 生产组下批量绑定质检班组
export function productAddInspectionTeam({ productLineId, groupId, teamIds }) {
  return request({
    module: 'bridge',
    url: 'groups/inspection_team',
    method: 'post',
    data: { id: groupId, teamIds }
  })
}

export default { get, add, edit, del }
