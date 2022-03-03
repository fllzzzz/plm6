import store from '@/store'
import { isBlank, isNotBlank } from '../data-type'
import { uniqueArr } from '../data-type/array'
import { measureTypeEnum } from '../enum/modules/wms'
import { rawMatClsEnum } from '@enum-ms/classification'

/**
 * 为列表设置规格
 * @param {array} list 需要转换的列表
 * @param {boolean} multipleSpec 多规格模式
 */
export async function setSpecInfoToList(list, { multipleSpec = false } = {}) {
  if (isBlank(list)) return list
  try {
    // 所有的promise
    const allPromise = []
    // 单个promise
    let p
    // 加载科目信息, 只处理有classifyId的数据
    const _list = list.filter((v) => v && v.classifyId)
    const classifyIds = uniqueArr(_list.map((v) => v.classifyId))
    await fetchSpecInfoByFullSpec(classifyIds)
    _list.forEach((row) => {
      if (isNotBlank(row.classifyId)) {
        let spec
        if (multipleSpec) {
          spec = Array.isArray(row.specifications) && row.specifications.length > 0 ? row.specifications[0] : ''
        } else {
          spec = row.specification
        }
        spec = isNotBlank(spec) ? spec : ''
        p = fetchSpecInfo(row.classifyId, spec).then((info) => {
          if (info) {
            // 单规格模式下，设置规格唯一编号
            if (!multipleSpec) {
              row.sn = info.sn // 该规格唯一编号
              row.serialNumber = info.serialNumber // 科目编号 - 规格
            }
            row.specificationLabels = info.specificationLabels // 规格中文
            row.classifySerialNumber = info.classify.serialNumber // 科目编号
            row.basicClass = info.classify.basicClass // 基础类型
            row.classifyId = info.classify.id // 科目id
            row.classifyFullPathId = info.classify.fullPathId // 全路径id
            row.classifyFullName = info.classify.fullName // 全路径名称
            row.classifyName = info.classify.name // 当前科目名称
            row.classifyParentFullName = info.classify.parentFullName // 父级路径名称
            row.measureUnit = info.classify.measureUnit // 计量单位
            row.measurePrecision = info.classify.measurePrecision // 计量单位小数精度
            row.accountingUnit = info.classify.accountingUnit // 核算单位
            row.accountingPrecision = info.classify.accountingPrecision // 核算单位小数精度
            row.outboundUnitType = info.classify.outboundUnitType // 出库方式
            row.outboundUnit = row.outboundUnitType === measureTypeEnum.MEASURE.V ? row.measureUnit : row.accountingUnit // 出库单位
            row.outboundUnitPrecision = row.outboundUnitType === measureTypeEnum.MEASURE.V ? row.measurePrecision : row.accountingPrecision // 出库单位精度

            row.rejectUnitType = row.basicClass === rawMatClsEnum.STEEL_COIL.V ? measureTypeEnum.ACCOUNTING.V : row.outboundUnitType // 退库方式
            row.rejectUnit = row.rejectUnitType === measureTypeEnum.MEASURE.V ? row.measureUnit : row.accountingUnit // 退库单位
            row.rejectUnitPrecision = row.rejectUnitType === measureTypeEnum.MEASURE.V ? row.measurePrecision : row.accountingPrecision // 退库单位精度
            // row.specification = info.spec // 规格
            row.specKV = info.specKV // 规格KV格式（例：key: 材质id ， val: 'Q235B'）
            row.specNameKV = info.specNameKV // 规格KV格式 （例：key: 材质 ， val: 'Q235B'）
            if (row.basicClass === rawMatClsEnum.SECTION_STEEL.V) {
              row.unitWeight = info.unitWeight // 单位重量 kg/m
            }
          }
        })
        allPromise.push(p)
      }
    })
    await Promise.all(allPromise)
  } catch (error) {
    console.log('加载科目报错', error)
  }

  return list
}

// 获取规格信息
export async function fetchSpecInfo(classifyId, spec) {
  // 加载科目
  // await fetchSpecInfoByFullSpec(classifyId)
  const classifySpec = store.state.config.classifySpec[classifyId]
  return classifySpec.fullSpecMap.get(spec)
}

// 根据规格全称 获取 完整的规格信息
async function fetchSpecInfoByFullSpec(classifyId) {
  const _classifyId = Array.isArray(classifyId) ? classifyId : [classifyId]
  const stateClassifySpec = store.state.config.classifySpec
  const unload = _classifyId.filter((id) => isBlank(stateClassifySpec[id]))
  // 拉取未加载的当前科目规格
  if (unload.length > 0) {
    unload.forEach((id) => {
      store.state.config.classifySpec[id] = {}
    })
    await store.dispatch('config/fetchMarClsSpec', unload)
  }
}
