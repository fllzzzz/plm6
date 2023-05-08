import request from '@/utils/request'

export function getArtifact(params) {
  return request({
    module: 'mes',
    url: 'package/artifact/use',
    method: 'get',
    params
  })
}

export function getEnclosure(params) {
  return request({
    url: '/api/mes/enclosure/package/enclosure/use',
    method: 'get',
    params
  })
}
// 直发件
export function getPart(params) {
  return request({
    module: 'mes',
    url: 'package/part/use',
    method: 'get',
    params
  })
}

export function getAuxiliaryMaterial(params) {
  return request({
    module: 'mes',
    url: 'package/auxiliary_material/use',
    method: 'get',
    params
  })
}

export function pack(data) {
  return request({
    module: 'mes',
    url: 'package',
    method: 'post',
    data
  })
}

// 围护：打包
export function enclosurePack(data) {
  return request({
    url: '/api/mes/enclosure/package',
    method: 'post',
    data
  })
}

export function editPack({ id, remark, packageLinks }) {
  return request({
    module: 'mes',
    url: `package`,
    method: 'put',
    data: { id, remark, packageLinks }
  })
}

// 围护
export function editEnclosurePack({ id, remark, packageLinks }) {
  return request({
    url: `/api/mes/enclosure/package`,
    method: 'put',
    data: { id, remark, packageLinks }
  })
}

export function additionalPack({ id, remark, packageLinks }) {
  return request({
    module: 'mes',
    url: `package/add`,
    method: 'put',
    data: { id, remark, packageLinks }
  })
}

// 围护
export function additionalEnclosurePack({ id, remark, packageLinks }) {
  return request({
    url: `/api/mes/enclosure/package/add`,
    method: 'put',
    data: { id, remark, packageLinks }
  })
}
