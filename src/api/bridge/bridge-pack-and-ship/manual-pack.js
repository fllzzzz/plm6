import request from '@/utils/request'

export function getArtifact(params) {
  return request({
    module: 'bridge',
    url: 'package/box/use',
    method: 'get',
    params
  })
}

export function getEnclosure(params) {
  return request({
    module: 'bridge',
    url: 'package/enclosure/use',
    method: 'get',
    params
  })
}

// export function getAuxiliaryMaterial(params) {
//   return request({
//     module: 'bridge',
//     url: 'package/auxiliary_material/use',
//     method: 'get',
//     params
//   })
// }

export function pack(data) {
  return request({
    module: 'bridge',
    url: 'package',
    method: 'post',
    data
  })
}

export function editPack({ id, remark, packageLinks }) {
  return request({
    module: 'bridge',
    url: `package`,
    method: 'put',
    data: { id, remark, packageLinks }
  })
}

export function additionalPack({ id, remark, packageLinks }) {
  return request({
    module: 'bridge',
    url: `package/add`,
    method: 'put',
    data: { id, remark, packageLinks }
  })
}
