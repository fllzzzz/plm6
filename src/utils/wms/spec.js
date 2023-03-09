import store from '@/store'
import { isBlank, isNotBlank } from '../data-type'
import { uniqueArr } from '../data-type/array'
import { measureTypeEnum } from '../enum/modules/wms'
import { rawMatClsEnum } from '@enum-ms/classification'
import { specFormat } from './spec-format'
import { STEEL_ENUM } from '@/settings/config'

/**
 * 为列表设置规格
 * @param {array} list 需要转换的列表
 * @param {boolean} multipleSpec 多规格模式
 * @param {string} prefix 对象前缀 如 list:[ {material:{}}] 实际要转换的是material,则prefix为material, 不支持多层嵌套
 */
export async function setSpecInfoToList(list, { multipleSpec = false, prefix } = {}) {
  if (isBlank(list)) return list
  let _list = list
  if (prefix) {
    _list = _list.map(item => item[prefix])
  }
  try {
    // 所有的promise
    const allPromise = []
    // 单个promise
    let p
    // 加载科目信息, 只处理有classifyId的数据
    _list = _list.filter((v) => v && v.classifyId)
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
        // 型材key:分类id_国标；其他材料key:分类id
        const fullSpecMapKey = row.nationalStandard ? `${row.classifyId}_${row.nationalStandard}` : row.classifyId
        p = fetchSpecInfo(fullSpecMapKey, spec).then((info) => {
          setSpecInfoForData(row, info, multipleSpec)
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
export async function fetchSpecInfo(fullSpecMapKey, spec) {
  // 加载科目
  // await fetchSpecInfoByFullSpec(classifyId)
  const classifySpec = store.state.config.classifySpec[fullSpecMapKey]
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

// 将材料规格信息设置进data
export function setSpecInfoForData(data, info, multipleSpec = false) {
  if (info) {
    // 单规格模式下，设置规格唯一编号
    if (!multipleSpec) {
      data.sn = info.sn // 该规格唯一编号
      data.serialNumber = info.serialNumber // 科目编号 - 规格
    }
    data.specMerge = specFormat(data) // 合并科目
    data.specificationLabels = info.specificationLabels // 规格中文
    data.classifySerialNumber = info.classify.serialNumber // 科目编号
    data.basicClass = info.classify.basicClass // 基础类型
    data.classifyId = info.classify.id // 科目id
    data.classifyFullPathId = info.classify.fullPathId // 全路径id
    data.classifyFullName = info.classify.fullName // 全路径名称
    // 分类（辅材、气体显示一级科目，钢板、钢卷、型材显示为钢材）
    data.classification = data.basicClass < STEEL_ENUM ? '钢材' : info.classify.fullPathName[0]
    data.classifyName = info.classify.name // 当前科目名称
    data.classifyParentFullName = info.classify.parentFullName // 父级路径名称
    data.measureUnit = isBlank(data.measureUnit) ? info.classify.measureUnit : data.measureUnit // 计量单位
    data.measurePrecision = isBlank(data.measurePrecision) ? info.classify.measurePrecision : data.measurePrecision // 计量单位小数精度
    data.accountingUnit = isBlank(data.accountingUnit) ? info.classify.accountingUnit : data.accountingUnit // 核算单位
    data.accountingPrecision = isBlank(data.accountingPrecision) ? info.classify.accountingPrecision : data.accountingPrecision // 核算单位小数精度
    data.outboundUnitType = info.classify.outboundUnitType // 出库方式
    data.outboundUnit = data.outboundUnitType === measureTypeEnum.MEASURE.V ? data.measureUnit : data.accountingUnit // 出库单位
    data.outboundUnitPrecision = data.outboundUnitType === measureTypeEnum.MEASURE.V ? data.measurePrecision : data.accountingPrecision // 出库单位精度

    data.rejectUnitType = data.basicClass === rawMatClsEnum.STEEL_COIL.V ? measureTypeEnum.ACCOUNTING.V : data.outboundUnitType // 退库方式
    data.rejectUnit = data.rejectUnitType === measureTypeEnum.MEASURE.V ? data.measureUnit : data.accountingUnit // 退库单位
    data.rejectUnitPrecision = data.rejectUnitType === measureTypeEnum.MEASURE.V ? data.measurePrecision : data.accountingPrecision // 退库单位精度
    // data.specification = info.spec // 规格
    data.specKV = info.specKV // 规格KV格式（例：key: 材质id ， val: 'Q235B'）
    data.specNameKV = info.specNameKV // 规格KV格式 （例：key: 材质 ， val: 'Q235B'）
    if (data.basicClass === rawMatClsEnum.SECTION_STEEL.V) {
      data.unitWeight = info.unitWeight // 单位重量 kg/m
    }
  }
}
