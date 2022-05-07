<template>
  <common-table
    v-if="props.show"
    ref="tableRef"
    :data="filterList"
    :data-format="columnsDataFormat"
    :height="props.height"
    :default-expand-all="false"
    :cell-class-name="wrongCellMask"
    highlight-current-row
    row-key="id"
  >
    <material-base-info-columns spec-merge fixed="left" />
    <el-table-column prop="material.brand" label="品牌" align="center" min-width="100px" fixed="left">
      <template #default="{ row: { sourceRow: row } }">
        <span v-if="row.lowerLimitNumber">{{ row.material.brand }}</span>
        <el-input v-else v-model.trim="row.material.brand" maxlength="60" size="mini" placeholder="品牌" />
      </template>
    </el-table-column>
    <!-- <el-table-column
      key="material.outboundUnit"
      prop="material.outboundUnit"
      label="计量单位"
      align="center"
      width="70px"
      fixed="left"
      show-overflow-tooltip
    />
    <el-table-column key="material.number" prop="material.number" label="数量" align="center" width="120px" show-overflow-tooltip>
      <template #default="{ row: { sourceRow: row } }">
        <el-tooltip effect="dark" :content="`当前记录已分拣：${row.material.lowerLimitNumber}`" :show-after="1000" placement="top-start">
          <common-input-number
            v-model="row.material.number"
            :min="row.material.lowerLimitNumber"
            :max="row.material.operableNumber"
            controls-position="right"
            :controls="false"
            :step="1"
            :precision="row.material.outboundUnitPrecision"
            size="mini"
            placeholder="数量"
            @change="(nv, ov) => handleNumberChange(nv, ov, row)"
          />
        </el-tooltip>
      </template>
    </el-table-column>
    <el-table-column
      key="material.theoryTotalWeight"
      prop="material.theoryTotalWeight"
      align="center"
      :label="`理论总重（kg）`"
      width="135px"
    /> -->
    <el-table-column prop="material.measureUnit" label="计量单位" align="center" min-width="70px" />
    <el-table-column prop="material.quantity" label="数量" align="center" min-width="120px">
      <template #default="{ row: { sourceRow: row } }">
        <template v-if="row.material.measureUnit">
          <el-tooltip
            effect="dark"
            :content="`当前记录已分拣：${row.material.lowerLimitQuantity}`"
            :show-after="1000"
            placement="top-start"
          >
            <common-input-number
              v-model="row.material.quantity"
              :min="row.material.lowerLimitQuantity"
              :max="999999999"
              :controls="false"
              :step="1"
              :precision="+row.material.measurePrecision"
              size="mini"
              placeholder="数量"
              @change="(nv, ov) => handleQuantityChange(nv, ov, row)"
            />
          </el-tooltip>
        </template>
        <span v-else>-</span>
      </template>
    </el-table-column>
    <el-table-column prop="material.accountingUnit" label="核算单位" align="center" min-width="70px" />
    <el-table-column prop="material.mete" label="核算量" align="center" min-width="120px">
      <template #default="{ row: { sourceRow: row } }">
        <el-tooltip
          v-if="crud.form.materialBasicClass === matClsEnum.MATERIAL.V"
          effect="dark"
          :content="`当前记录已分拣：${row.material.lowerLimitMete}`"
          :show-after="1000"
          placement="top-start"
        >
          <common-input-number
            v-model="row.material.mete"
            :min="row.material.lowerLimitMete"
            :max="999999999"
            :controls="false"
            :step="1"
            :precision="+row.material.accountingPrecision"
            size="mini"
            placeholder="核算量"
            @change="(nv, ov) => handleMeteChange(nv, ov, row)"
          />
        </el-tooltip>
        <span v-else>{{ row.material.mete || 0 }}</span>
      </template>
    </el-table-column>
    <el-table-column key="remark" prop="remark" align="center" label="备注" min-width="150px">
      <template #default="{ row: { sourceRow: row } }">
        <el-input v-model="row.remark" placeholder="备注" maxlength="200" size="mini" />
      </template>
    </el-table-column>
    <el-table-column label="操作" width="70" align="center" fixed="right">
      <template #default="{ row: { sourceRow: row }, $index }">
        <common-button
          class="icon-button"
          icon="el-icon-delete"
          type="danger"
          size="mini"
          :disabled="row.material.lowerLimitNumber > 0"
          @click="delRow(row, $index)"
        />
      </template>
    </el-table-column>
  </common-table>
</template>

<script setup>
import { defineExpose, defineProps, reactive, ref, computed, nextTick } from 'vue'
import { measureTypeEnum } from '@/utils/enum/modules/wms'
import { matClsEnum } from '@/utils/enum/modules/classification'
import { componentTypeEnum } from '@/utils/enum/modules/building-steel'
import { STEEL_BASE_UNIT, STEEL_ENUM } from '@/settings/config'
import { deepClone, isBlank, isNotBlank, toPrecision } from '@/utils/data-type'
import { setSpecInfoToList } from '@/utils/wms/spec'
import { numFmtByBasicClass } from '@/utils/wms/convert-unit'
import { calcTheoryWeight } from '@/utils/wms/measurement-calc'
import { createUniqueString } from '@/utils/data-type/string'
import cloneDeep from 'lodash/cloneDeep'

import { regExtra } from '@compos/use-crud'
import MaterialBaseInfoColumns from '@/components-system/wms/table-custom-field-columns/material-base-info-columns/index.vue'
import useTableValidate from '@/composables/form/use-table-validate'

const props = defineProps({
  show: {
    type: Boolean
  },
  height: {
    type: Number,
    default: 400
  },
  currentTechRow: {
    // 当前选中的清单
    type: Object
  },
  queryFilter: {
    // 查询过滤
    type: Object,
    default: () => ({})
  }
})
// -------------------------------------------------------------------------------------------------------------------------------------
const purchaseList = ref([])

// 根据查询条件过滤列表
const filterList = computed(() => {
  let list = purchaseList.value
  if (props.queryFilter.showOnlySelectTechInfo && props.currentTechRow) {
    list = list.filter((row) => row.boundTech.id === props.currentTechRow.id)
  }
  return list
})

// 表格列数据格式转换
const columnsDataFormat = [['material.mete', ['to-fixed-field', 'accountingUnit']]]

// 数量校验方式
const validateQuantity = (value, row) => {
  if (row.material.measureUnit) return !!value && value > 0
  return true
}

// 核算量校验
const validateMete = (value, row) => {
  if (!row.material.measureUnit) {
    return !!value && value > 0
  }
  return true
}

// 表格规则
const tableRules = {
  'material.mete': [{ validator: validateMete, message: '核算量必须大于0' }],
  'material.quantity': [{ validator: validateQuantity, message: '有计量单位，数量必须大于0' }]
}

// 表格校验
const { tableValidate, wrongCellMask } = useTableValidate({ rules: tableRules, errorMsg: '请修正【需要采购清单】中标红的信息' }) // 表格校验

// 校验
function validate() {
  if (isBlank(purchaseList.value)) return true
  const { validResult, dealList } = tableValidate(purchaseList.value)
  purchaseList.value = dealList
  // 为表单赋值
  crud.form.purchaseList = dealList
  return validResult
}

// -------------------------------------------------------------------------------------------------------------------------------------
// 获取crud实例，并将实例注册进crud
const { CRUD, crud } = regExtra()

// 初始化表单
CRUD.HOOK.beforeEditDetailLoaded = async (crud, form) => {
  // 设置需采购清单
  purchaseList.value = await listFM(deepClone(form.purchaseList || []))
  // 绑定清单汇总列表
  initialBindingTechnologyList(purchaseList.value, form.technologyList)
  // 计算清单备料量及差值
  initialCalcTechPrepMete(purchaseList.value, form.technologyList)
  // 计算需采购清单总量
  calcPurchaseTotalMete(purchaseList.value)
}

// 添加
function addRow(row, technologyRow) {
  // 转换
  crud.props.inventoryExitIdMap.set(row.material.id, true)
  const purRow = reactive(cloneDeep(row))
  purRow.boolAdd = true // 是否添加
  purRow.id = createUniqueString() // 设置记录id（假，用于绑定）
  // 技术清单与库存利用清单互相绑定
  purRow.boundTech = technologyRow
  technologyRow.boundPurIds.push(purRow.id)
  // 监听
  // rowWatch(invRow)
  calcSteelTotalWeight(purRow.material)
  purchaseList.value.push(purRow)
  // 若有数量，则计算该物料清单备料量
  if (purRow.material.number) {
    handleNumberChange(purRow.material.number, undefined, purRow)
  }
}

// 删除行
function delRow(row, index) {
  crud.props.inventoryExitIdMap.set(row.material.id, false)
  purchaseList.value.splice(index, 1)
  // 解除技术清单与需要采购清单之间的绑定
  const techRow = row.boundTech
  techRow.boundPurIds.remove(row.id)
  // 重新计算备料量
  calcTechPrepMete(purchaseList.value, techRow)
  // 重新计算需要采购总量
  calcPurchaseTotalMete(purchaseList.value)
}

// 处理量的变换
function handleNumberChange(newVal, oldVal, row) {
  if (row.material.outboundUnitType === measureTypeEnum.MEASURE.V) {
    handleQuantityChange(newVal, oldVal, row)
  } else {
    handleMeteChange(newVal, oldVal, row)
  }
}

// 处理数量变化
function handleQuantityChange(newVal, oldVal, row) {
  const { material } = row
  // 填写的字段
  const triggerCalc = () => {
    if (oldVal !== material.quantity) {
      if (material.basicClass & STEEL_ENUM) {
        calcSteelTotalWeight(material)
      }
      calcTechPrepMete(purchaseList.value, row.boundTech)
      calcPurchaseTotalMete(purchaseList.value)
    }
  }

  if (!newVal || newVal < material.lowerLimitQuantity) {
    // 设置为空 或 小于最小数量，则设置为最小数量
    nextTick(() => {
      material.quantity = material.lowerLimitQuantity
      triggerCalc()
    })
  } else {
    triggerCalc()
  }
}

// 处理核算量变化
function handleMeteChange(newVal, oldVal, row) {
  const { material } = row
  // 填写的字段
  const triggerCalc = () => {
    if (oldVal !== material.mete) {
      calcTechPrepMete(purchaseList.value, row.boundTech)
      calcPurchaseTotalMete(purchaseList.value)
    }
  }

  if (!newVal || newVal < material.lowerLimitMete) {
    // 设置为空 或 小于最小数量，则设置为最小数量
    nextTick(() => {
      material.mete = material.lowerLimitMete
      triggerCalc()
    })
  } else {
    triggerCalc()
  }
}

// 计算需要采购总量
function calcPurchaseTotalMete(purchaseList) {
  if (isNotBlank(purchaseList)) {
    crud.props.purchaseTotalMete = purchaseList.reduce((sum, cur) => {
      return (sum += cur.material.mete || 0)
    }, 0)
  } else {
    crud.props.purchaseTotalMete = 0
  }
}

// 需采购清单格式转换
async function listFM(list) {
  if (!list) return []
  // 格式装换
  await setSpecInfoToList(list, { prefix: 'material' })
  await numFmtByBasicClass(
    list,
    {
      prefix: 'material',
      toNum: true
    },
    {
      mete: ['mete', 'sortingMete'],
      quantity: ['quantity', 'sortingQuantity']
    }
  )
  // 计算理论重量
  if (crud.form.materialBasicClass & STEEL_ENUM) {
    await calcTheoryWeight(list, { prefix: 'material' })
  }

  // 数量处理
  list.forEach((row) => {
    const { material } = row
    if (material.outboundUnitType === measureTypeEnum.MEASURE.V) {
      // 实际在出库中使用的数量
      material.number = material.quantity // 需采购数量
      material.sortingNumber = material.sortingQuantity // 分拣数量
      material.lowerLimitQuantity = material.sortingQuantity // 最小数量(分拣数量)
    } else {
      // 核算量
      material.number = material.mete // 需采购核算量
      material.sortingNumber = material.sortingMete // 分拣核算量
      material.lowerLimitMete = material.sortingMete // 最小核算量(分拣核算量)
    }
    material.operableNumber = 999999999 // 最大数量
    material.lowerLimitNumber = material.sortingNumber // 最小数量
    row = reactive(row)

    // 计算理论重量
    if (material.basicClass & STEEL_ENUM) {
      calcSteelTotalWeight(material)
    }
    // rowWatch(row)
  })
  return list
}

// 初始-绑定清单汇总列表
function initialBindingTechnologyList(purchaseList, technologyList) {
  // 类型是钢板或者型钢
  if (crud.form.technologyListType === componentTypeEnum.STRUCTURE.V) {
    const steelClassifyConfICKV = crud.props.steelClassifyConfICKV // 钢材配置
    purchaseList.forEach((purRow) => {
      const { material } = purRow
      for (const techRow of technologyList) {
        const boundClassifyIds = steelClassifyConfICKV[techRow.steelClassifyConfId] || []
        // ------------- 钢板 ------------------
        if (
          material.basicClass === matClsEnum.STEEL_PLATE.V && // 钢板
          boundClassifyIds.includes(material.classifyId) && // 属于绑定分类
          material.specNameKV['材质'] === techRow.material && // 材质相同
          `${material.theoryThickness}` === techRow.specification // 厚度相同
        ) {
          purRow.boundTech = techRow
          techRow.boundPurIds.push(purRow.id)
          break
        }
        // ------------- 型材 ------------------
        if (
          material.basicClass === matClsEnum.SECTION_STEEL.V && // 型材
          boundClassifyIds.includes(material.classifyId) && // 属于绑定分类
          material.specNameKV['材质'] === techRow.material && // 材质相同
          material.specNameKV[material.nationalStandard] === techRow.specification // 规格相同
        ) {
          purRow.boundTech = techRow
          techRow.boundPurIds.push(purRow.id)
          break
        }
      }
    })
  }
}

/**
 * 初始-计算清单备料量
 * @param {array} purchaseList 需采购清单
 * @param {array} technologyList 技术汇总清单
 * @param {number} materialBasicClass 备料基础分类（bit）
 */
function initialCalcTechPrepMete(purchaseList, technologyList) {
  technologyList.forEach((techRow) => {
    // 初始化赋值
    crud.props.techPrepMeteKV[techRow.id] = crud.props.techPrepMeteKV[techRow.id] || {}
    // 计算当前技术清单行的备料量
    calcTechPrepMete(purchaseList, techRow)
  })
}

/**
 * 计算清单备料量
 * @param {array} purchaseList 需采购清单
 * @param {object} technologyRow 技术汇总清单 行数据
 * @param {number} materialBasicClass 备料基础分类（bit）
 */
function calcTechPrepMete(purchaseList, technologyRow) {
  const info = crud.props.techPrepMeteKV[technologyRow.id]
  const boundMaterial = [] // 绑定物料的数组
  // 遍历“技术清单行”绑定的需采购清单id
  technologyRow.boundPurIds.forEach((id) => {
    // 遍历“需采购清单”，将绑定物料推入“绑定物料的数组”
    for (const purRow of purchaseList) {
      if (purRow.id === id) {
        boundMaterial.push(purRow.material)
        break
      }
    }
  })
  // -------------- 钢板或型钢 ----------------
  if (crud.form.technologyListType === componentTypeEnum.STRUCTURE.V) {
    // 累加理论重量
    const summary = boundMaterial.reduce((sum, cur) => {
      return (sum += cur.mete || 0)
    }, 0)

    info.purchase = toPrecision(summary, STEEL_BASE_UNIT.weight.precision) // 库存利用量
    info.preparation = toPrecision((info.inventory || 0) + (info.purchase || 0), STEEL_BASE_UNIT.weight.precision) // 总备料量
    info.diff = toPrecision(info.preparation - technologyRow.listMete, STEEL_BASE_UNIT.weight.precision) // 差值 = 总备料量 - 清单量
    info.isEnough = info.diff >= 0 // 是否超出
  }
}

// 计算钢板总重
function calcSteelTotalWeight(material) {
  if (isNotBlank(material.theoryWeight) && material.quantity) {
    material.mete = material.theoryWeight * material.quantity
  } else {
    material.mete = undefined
  }
}

defineExpose({
  add: addRow,
  validate: validate
})
</script>
