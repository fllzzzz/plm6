<template>
  <common-table
    v-if="props.show"
    ref="tableRef"
    :data="purchaseList"
    :height="props.height"
    :default-expand-all="false"
    highlight-current-row
    row-key="recordId"
  >
    <material-base-info-columns spec-merge fixed="left" />
    <el-table-column prop="brand" label="品牌" align="center" min-width="100px" fixed="left">
      <template #default="{ row }">
        <el-input v-model.trim="row.brand" maxlength="60" size="mini" placeholder="品牌" />
      </template>
    </el-table-column>
    <!-- 单位及其数量 -->
    <!-- <material-unit-quantity-columns :basic-class="basicClass" /> -->
    <el-table-column key="outboundUnit" prop="outboundUnit" label="计量单位" align="center" width="70px" fixed="left" show-overflow-tooltip>
      <template #default="{ row }">
        <span v-empty-text>{{ row.outboundUnit }}</span>
      </template>
    </el-table-column>
    <el-table-column key="number" prop="number" label="数量" align="center" width="120px" show-overflow-tooltip>
      <template #default="{ row }">
        <el-tooltip effect="dark" :content="`当前记录已分拣：${row.lowerLimitNumber}`" :show-after="1000" placement="top-start">
          <common-input-number
            v-model="row.number"
            :min="row.lowerLimitNumber"
            :max="row.operableNumber"
            controls-position="right"
            :controls="false"
            :step="1"
            :precision="row.outboundUnitPrecision"
            size="mini"
            placeholder="数量"
            @change="handleNumberChange(row)"
          />
        </el-tooltip>
      </template>
    </el-table-column>
    <el-table-column key="theoryTotalWeight" prop="theoryTotalWeight" align="center" :label="`理论总重（kg）`" width="135px">
      <template #default="{ row }">
        <span v-to-fixed="{ val: row.theoryTotalWeight, k: 'COM_WT__KG' }" v-empty />
      </template>
    </el-table-column>
    <el-table-column key="remark" prop="remark" align="center" label="备注" min-width="150px">
      <template #default="{ row }">
        <el-input v-model="row.remark" placeholder="备注" maxlength="200" size="mini" />
      </template>
    </el-table-column>
    <el-table-column label="操作" width="70" align="center" fixed="right">
      <template #default="{ row, $index }">
        <common-button
          class="icon-button"
          icon="el-icon-delete"
          type="danger"
          size="mini"
          :disabled="row.lowerLimitNumber > 0"
          @click="delRow(row, $index)"
        />
      </template>
    </el-table-column>
  </common-table>
</template>

<script setup>
import { defineExpose, defineProps, reactive, ref, computed } from 'vue'
import { deepClone, isNotBlank, toPrecision } from '@/utils/data-type'
import { setSpecInfoToList } from '@/utils/wms/spec'
import { numFmtByBasicClass } from '@/utils/wms/convert-unit'
import { calcTheoryWeight } from '@/utils/wms/measurement-calc'
import { measureTypeEnum } from '@/utils/enum/modules/wms'
import { matClsEnum } from '@/utils/enum/modules/classification'
import { DP, STEEL_ENUM } from '@/settings/config'

import { regExtra } from '@compos/use-crud'
import MaterialBaseInfoColumns from '@/components-system/wms/table-columns/material-base-info-columns/index.vue'

const props = defineProps({
  show: {
    type: Boolean
  },
  height: {
    type: Number,
    default: 400
  }
})

const purchaseList = ref([])
// 是钢板或者型钢
const isSPorSS = computed(
  () => crud.form.materialBasicClass & matClsEnum.STEEL_PLATE.V || crud.form.materialBasicClass & matClsEnum.SECTION_STEEL.V
)
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
function addRow(row) {
  console.log(row)
}

// 删除行
function delRow(row, index) {
  crud.props.inventoryExitIdMap.set(row.id, false)
  purchaseList.value.splice(index, 1)
  // 解除技术清单与需要采购清单之间的绑定
  const techRow = row.boundTech
  techRow.boundPurIds.remove(row.recordId)
  // 重新计算备料量
  calcTechPrepMete(purchaseList.value, techRow)
  // 重新计算需要采购总量
  calcPurchaseTotalMete(purchaseList.value)
}

// 处理数量变化
function handleNumberChange(row) {
  if (row.basicClass & STEEL_ENUM) {
    calcSteelTotalWeight(row)
  }
  calcTechPrepMete(purchaseList.value, row.boundTech)
  calcPurchaseTotalMete(purchaseList.value)
}

// 计算需要采购总量
function calcPurchaseTotalMete(purchaseList) {
  if (isNotBlank(purchaseList)) {
    // 钢板或型材
    if (isSPorSS.value) {
      crud.props.purchaseTotalMete = purchaseList.reduce((sum, cur) => {
        return (sum += cur.theoryTotalWeight || 0)
      }, 0)
    }
  } else {
    crud.props.purchaseTotalMete = 0
  }
}

// 需采购清单格式转换
async function listFM(list) {
  if (!list) return []
  // 格式装换
  await setSpecInfoToList(list)
  await numFmtByBasicClass(list, {
    toNum: true
  })
  // 计算理论重量
  await calcTheoryWeight(list)

  // 数量处理
  list.forEach((row) => {
    if (row.outboundUnitType === measureTypeEnum.MEASURE.V) {
      // 实际在出库中使用的数量
      row.number = row.quantity // 需采购数量
      row.sortingNumber = row.sortingQuantity // 分拣数量
    } else {
      // 核算量
      row.number = row.mete // 需采购核算量
      row.sortingNumber = row.sortingMete // 分拣核算量
    }
    row.operableNumber = 999999999 // 最大数量
    row.lowerLimitNumber = row.sortingNumber // 最小数量
    row = reactive(row)
    // 计算理论重量
    if (row.basicClass & STEEL_ENUM) {
      calcSteelTotalWeight(row)
    }
    // rowWatch(row)
  })
  return list
}

// 初始-绑定清单汇总列表
function initialBindingTechnologyList(purchaseList, technologyList) {
  // 类型是钢板或者型钢
  if (isSPorSS.value) {
    const steelClassifyConfICKV = crud.props.steelClassifyConfICKV // 钢材配置
    purchaseList.forEach((purRow) => {
      for (const techRow of technologyList) {
        const boundClassifyIds = steelClassifyConfICKV[techRow.steelClassifyConfId] || []
        // ------------- 钢板 ------------------
        if (
          purRow.basicClass === matClsEnum.STEEL_PLATE.V && // 钢板
          boundClassifyIds.includes(purRow.classifyId) && // 属于绑定分类
          purRow.specNameKV['材质'] === techRow.material && // 材质相同
          `${purRow.theoryThickness}` === techRow.specification // 厚度相同
        ) {
          purRow.boundTech = techRow
          techRow.boundPurIds.push(purRow.recordId)
          break
        }
        // ------------- 型材 ------------------
        if (
          purRow.basicClass === matClsEnum.SECTION_STEEL.V && // 型材
          boundClassifyIds.includes(purRow.classifyId) && // 属于绑定分类
          purRow.specNameKV['材质'] === techRow.material && // 材质相同
          purRow.specNameKV[purRow.nationalStandard] === techRow.specification // 规格相同
        ) {
          purRow.boundTech = techRow
          techRow.boundPurIds.push(purRow.recordId)
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
      if (purRow.recordId === id) {
        boundMaterial.push(purRow)
        break
      }
    }
  })
  // -------------- 钢板或型钢 ----------------
  if (isSPorSS.value) {
    // 累加理论重量
    const summary = boundMaterial.reduce((sum, cur) => {
      return (sum += cur.theoryTotalWeight || 0)
    }, 0)

    info.purchase = toPrecision(summary, DP.COM_WT__KG) // 库存利用量
    info.preparation = toPrecision((info.inventory || 0) + (info.purchase || 0), DP.COM_WT__KG) // 总备料量
    info.diff = toPrecision(info.preparation - technologyRow.listMete, DP.COM_WT__KG) // 差值 = 总备料量 - 清单量
    info.isEnough = info.diff >= 0 // 是否超出
  }
}

// 计算钢板总重
function calcSteelTotalWeight(row) {
  if (isNotBlank(row.theoryWeight) && row.number) {
    row.theoryTotalWeight = row.theoryWeight * row.number
  } else {
    row.theoryTotalWeight = undefined
  }
}

defineExpose({
  add: addRow
})
</script>
