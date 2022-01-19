import request from '@/utils/request'

export function getStructure(params) {
  return request({
    module: 'plan',
    url: 'product/artifact/project',
    method: 'get',
    params
  })
}

export function getPart(params) {
  return request({
    module: 'plan',
    url: 'product/part/project',
    method: 'get',
    params
  })
}

// export function getEnclosure(params) {
//   return request({
//     module: 'plan',
//     url: 'product/enclosure/project',
//     method: 'get',
//     params
//   })
// }

export function getEnclosure(params) {
  return request({
    module: 'plan',
    url: 'enclosure/listSum',
    method: 'get',
    params
  })
}

export function structureMonomer(params) {
  return request({
    module: 'plan',
    url: 'product/artifact/monomer',
    method: 'get',
    params
  })
}

export function partMonomer(params) {
  return request({
    module: 'plan',
    url: 'product/part/monomer',
    method: 'get',
    params
  })
}

export function enclosureMonomer(params) {
  return request({
    module: 'plan',
    url: 'product/enclosure/monomer',
    method: 'get',
    params
  })
}
