import store from '@/store/index'
import { deepClone } from '@/utils/data-type'
import { createUniqueString, trimStr } from '@/utils/data-type/string'
import { matClsEnum } from '@/utils/enum/modules/classification'
import { calcSteelCoilLength } from '@/utils/wms/measurement-calc'
import { dataValidate } from '@/composables/form/use-table-validate'
import { compareArray } from '@/utils/data-type/array'
import { MAT_BASE_UNIT } from '@/settings/config'

const baseUnit = MAT_BASE_UNIT[matClsEnum.STEEL_COIL.V]
const sectionSteelSpecTmpl = {
  title: '钢卷申购清单', // 表格名称
  dlgWidth: '1500px', // 预览窗口宽度
  startRow: 2, // 起始行
  endIgnoreRow: 1, // 忽略行 如合计
  // 解析字段
  fields: [
    { label: '物料种类', field: 'classifyName', excelField: '__EMPTY' },
    { label: '材质', field: 'material', excelField: '__EMPTY_1' },
    { label: '厚（mm）', field: 'thickness', type: 'number', precision: baseUnit.thickness.precision, excelField: '__EMPTY_2' },
    { label: '宽（mm）', field: 'width', type: 'number', precision: baseUnit.width.precision, excelField: '__EMPTY_3' },
    { label: '长（mm）', field: 'length', type: 'number', precision: baseUnit.length.precision, excelField: '__EMPTY_4' },
    { label: '总重（kg）', field: 'weighingTotalWeight', type: 'number', precision: baseUnit.weight.precision, excelField: '__EMPTY_5' },
    { label: '品牌', field: 'brand', excelField: '__EMPTY_6' }
  ],
  // 校验规则
  rules: {
    classifyName: [{ required: true, message: '物料种类不能为空' }]
  },
  // 提交前的数据格式转换
  format: async (tableList) => {
    // 校验
    const validate = dataValidate(tableList, sectionSteelSpecTmpl.rules)
    if (!validate) return
    // 加载科目树
    if (!store.state.config.loaded.matClsTree) {
      await store.dispatch('config/fetchMatClsTree')
    }
    // 获取 钢板科目
    const matList = store.state.config.rawMatClsList.filter((v) => v.basicClass === matClsEnum.STEEL_COIL.V)
    const rawMatClsKV = store.state.config.rawMatClsKV
    // 遍历导入表格
    const list = deepClone(tableList)
    // 匹配到的id
    const existClassifyIds = []
    // 不存在的科目名称
    let unexistClassifyName = []
    // 遍历获取科目名称对应的id
    list.forEach((row) => {
      let exist = false
      // 遍历 钢板科目，寻找与导入名称相同的科目
      // 处理 多层级名称的情况 A/B/C
      const nameArr = trimStr(row.classifyName.split('/'))
      for (const mat of matList) {
        // 1.科目全路径数量必须>=导入的名称数量；2.从数组反向截取比较是否相同
        const flag = mat.fullPathName.length >= nameArr.length && compareArray(mat.fullPathName.slice(-nameArr.length), nameArr)
        if (flag) {
          exist = true
          row.classifyId = mat.id // 设置科目id
          existClassifyIds.push(mat.id)
          break
        }
      }
      if (!exist) unexistClassifyName.push(row.classifyName)
    })
    // 去重
    unexistClassifyName = Array.from(new Set(unexistClassifyName))
    // 有不存在的科目时，视为导入失败
    if (unexistClassifyName.length > 0) {
      throw new Error(`${unexistClassifyName.map((v) => `“${v}”`).join('、')}${unexistClassifyName.length > 1 ? '等' : ''}物料种类不存在`)
    }
    // 根据科目id获取对应科目的规格并做匹配
    const stateClassifySpec = store.state.config.classifySpec
    const unloadClassifyIds = existClassifyIds.filter((id) => stateClassifySpec[id] === undefined)
    // 拉取未加载的当前科目规格
    if (unloadClassifyIds.length > 0) {
      unloadClassifyIds.forEach((id) => {
        stateClassifySpec[id] = {}
      })
      await store.dispatch('config/fetchMarClsSpec', unloadClassifyIds)
    }
    // 匹配规格
    for (const row of list) {
      // 必须是末级科目
      if (rawMatClsKV?.[row.classifyId]?.isLeaf === false) {
        throw new Error(`${row.classifyName}不是末级科目`)
      }
      const materialInfo = stateClassifySpec[row.classifyId]
      // 未配置核算单位
      if (materialInfo.hasUnitConfig === false) {
        throw new Error(`${row.classifyName}未配置核算单位`)
      }
      // 当前规格的物料信息
      let materialSpecInfo
      const specification = row.material ?? '' // 对应申购“规格”列
      if (materialInfo && Array.isArray(materialInfo.specList)) {
        for (const specInfo of materialInfo.specList) {
          if (specification === specInfo.spec) {
            materialSpecInfo = specInfo
            break
          }
        }
      }
      // 存在物料规格，则设置物料信息
      if (materialSpecInfo) {
        row.uid = createUniqueString() // 临时唯一编码
        row.sn = materialSpecInfo.sn // 该科目规格唯一编号
        row.specificationLabels = materialSpecInfo.specificationLabels // 规格中文
        row.serialNumber = materialSpecInfo.serialNumber // 科目编号 - 规格
        row.classifyId = materialSpecInfo.classify.id // 科目id
        row.classifyFullName = materialSpecInfo.classify.fullName // 全路径名称
        row.classifyName = materialSpecInfo.classify.name // 当前科目名称
        row.classifyParentFullName = materialSpecInfo.classify.parentFullName // 父级路径名称
        row.basicClass = materialSpecInfo.classify.basicClass // 基础类型
        row.specification = materialSpecInfo.spec // 规格
        row.specificationMap = materialSpecInfo.specKV // 规格KV格式
        row.measureUnit = materialSpecInfo.classify.measureUnit // 计量单位
        row.accountingUnit = materialSpecInfo.classify.accountingUnit // 核算单位
        row.accountingPrecision = materialSpecInfo.classify.accountingPrecision // 核算单位小数精度
        row.measurePrecision = materialSpecInfo.classify.measurePrecision // 计量单位小数精度

        // 设置理论长度
        await calcTheoryLength(row)

        // 设置总长度
        row.length = row.length ?? row.theoryLength
        row.quantity = row.length
        row.mete = row.weighingTotalWeight
      } else {
        throw new Error(`${row.classifyName}下不存在规格为“${specification}”的材料，请联系管理员/仓库人员添加`)
      }
    }
    return list
  }
}

// 总重计算与单位重量计算分开，避免修改数量时需要重新计算单件重量
// 计算单件重量
async function calcTheoryLength(row) {
  row.theoryLength = await calcSteelCoilLength({
    name: row.classifyFullName,
    weight: row.weighingTotalWeight,
    width: row.width,
    thickness: row.thickness
  })
}

export default sectionSteelSpecTmpl
