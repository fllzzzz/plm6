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
    module: 'mes',
    url: 'package/enclosure/use',
    method: 'get',
    params
  })
}

// export function getAuxiliaryMaterial(params) {
//   return request({
//     module: 'mes',
//     url: 'package/auxiliary_material/use',
//     method: 'get',
//     params
//   })
// }

export function pack(data) {
  return request({
    module: 'mes',
    url: 'package',
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

export function additionalPack({ id, remark, packageLinks }) {
  return request({
    module: 'mes',
    url: `package/add`,
    method: 'put',
    data: { id, remark, packageLinks }
  })
}