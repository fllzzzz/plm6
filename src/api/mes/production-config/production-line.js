import request from '@/utils/request'

export function get(params) {
  return request({
    module: 'mes',
    url: 'productionLine/page',
    method: 'get',
    params
  })
}

export function add(data) {
  return request({
    module: 'mes',
    url: 'productionLine',
    method: 'post',
    data
  })
}

export function edit(data) {
  return request({
    module: 'mes',
    url: 'productionLine',
    method: 'put',
    data
  })
}

export function del(ids) {
  return request({
    module: 'mes',
    url: 'productionLine',
    method: 'delete',
    data: {
      ids: ids
    }
  })
}

export function editStatus({ id, boolEnabledEnum }) {
  return request({
    module: 'mes',
    url: 'productionLine/changeState',
    method: 'put',
    data: { id, boolEnabledEnum }
  })
}

export function productConfigInfo({ productType, boolMachineEnum }) {
  return request({
    module: 'mes',
    url: 'productionLine/type',
    method: 'get',
    params: { productType, boolMachineEnum }
  })
}

// 生产线下批量绑定班组
export function productAddTeam({ productLineId, teamIds }) {
  return request({
    module: 'mes',
    url: 'productionLine/team',
    method: 'post',
    data: { id: productLineId, teamIds }
  })
}

// 生产线下批量绑定质检班组
export function productAddInspectionTeam({ productLineId, teamIds }) {
  return request({
    module: 'mes',
    url: 'productionLine/inspection_team',
    method: 'post',
    data: { id: productLineId, teamIds }
  })
}

export default { get, add, edit, del }
