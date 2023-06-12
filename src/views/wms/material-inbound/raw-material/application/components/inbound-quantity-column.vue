<template>
  <el-table-column prop="quantity" align="center" width="110px" :label="`本次实收数 (${baseUnit.measure.unit})`">
    <template #default="{ row }">
      <template v-if="!row.boolApplyPurchase">
        <common-input-number
          v-if="!row.boolApplyPurchase && form.selectObj?.[row.mergeId]?.isSelected"
          v-model="row.quantity"
          :min="0"
          :max="999999999"
          controls-position="right"
          :controls="false"
          :step="1"
          :precision="baseUnit.measure.precision"
          size="mini"
          placeholder="实收数"
          @blur="handleOverQuantity(row)"
        />
        <span v-else>{{ row.quantity || '-' }}</span>
      </template>
      <template v-else>
        <span>{{ row.quantity }}</span>
        <span style="margin-left: 5px; cursor: pointer" @click="showDialog(row)">
          <el-icon-edit v-if="form.selectObj?.[row.mergeId]?.isSelected" class="el-icon" style="color: #1881ef; vertical-align: middle" />
          <el-icon-view v-else class="el-icon" style="color: #1881ef; vertical-align: middle" />
        </span>
      </template>
    </template>
  </el-table-column>
  <common-dialog
    :title="`${curRow.classifyName}：${curRow.serialNumber}`"
    v-model="dialogVisible"
    width="90%"
    :before-close="
      () => {
        dialogVisible = false
      }
    "
    :show-close="false"
    :close-on-click-modal="true"
    custom-class="inbound-requisition-dialog"
    top="10vh"
  >
    <common-table
      :data="curRow.applyPurchase"
      :maxHeight="maxHeight"
      style="width: 100%"
      :span-method="spanMethod"
      return-source-data
      :show-empty-symbol="false"
    >
      <el-table-column label="序号" type="index" align="center" width="60" />
      <el-table-column prop="applyPurchaseSN" :show-overflow-tooltip="true" label="申购单号" min-width="140" align="center" />
      <el-table-column prop="project" :show-overflow-tooltip="true" label="项目" min-width="140" align="center">
        <template #default="{ row: purRow }">
          <span v-if="purRow.project">{{ projectNameFormatter(purRow.project) }}</span>
          <span v-else>-</span>
        </template>
      </el-table-column>
      <el-table-column
        prop="purchaseQuantity"
        :show-overflow-tooltip="true"
        :label="`采购数量 (${baseUnit.measure.unit})`"
        width="100"
        align="center"
      />
      <el-table-column
        prop="inboundQuantity"
        :show-overflow-tooltip="true"
        :label="`已入库数量 (${baseUnit.measure.unit})`"
        width="110"
        align="center"
      />
      <slot name="editColumn" :row="curRow" />
      <el-table-column
        v-if="form.selectObj?.[curRow.mergeId]?.isSelected"
        prop="quantity"
        :show-overflow-tooltip="true"
        :label="`本次实收数 (${baseUnit.measure.unit})`"
        min-width="100px"
        align="center"
      >
        <template #default="{ row: purRow, $index }">
          <common-input-number
            v-model="purRow.quantity"
            :min="0"
            :max="999999999"
            controls-position="right"
            :controls="false"
            :step="1"
            :precision="baseUnit.measure.precision"
            size="mini"
            placeholder="实收数"
            @change="
              () => {
                purRow.comparePurchaseQuantity = purRow.purchaseQuantity - branchQuantityArr[$index] + purRow.quantity
              }
            "
            @blur="handleOverQuantity(purRow, true, 'comparePurchaseQuantity')"
          />
        </template>
      </el-table-column>
      <el-table-column prop="theoryTotalWeight" align="center" :label="`理论重量 (${baseUnit.weight.unit})`" min-width="100px" />
      <el-table-column v-if="form.selectObj?.[curRow.mergeId]?.isSelected" label="操作" width="100px" align="center" fixed="right">
        <template #default="{ row: purRow, $index }">
          <common-button
            v-if="purRow.isBranch || purRow.spanMerge > 1"
            icon="el-icon-delete"
            type="danger"
            size="mini"
            class="icon-button"
            @click="delRow($index, curRow.applyPurchase, purRow)"
          />
          <common-button
            icon="el-icon-plus"
            type="success"
            size="mini"
            class="icon-button"
            @click="addRow($index, curRow.applyPurchase, purRow,)"
          />
        </template>
      </el-table-column>
    </common-table>
  </common-dialog>
</template>

<script setup>
import { defineProps, ref, reactive, computed, watch } from 'vue'
import { matClsEnum } from '@/utils/enum/modules/classification'
import { projectNameFormatter } from '@/utils/project'
import { isNotBlank, toPrecision } from '@/utils/data-type'
import { calcSectionSteelWeight, calcSteelPlateWeight } from '@/utils/wms/measurement-calc'
import useMaxHeight from '@compos/use-max-height'

const props = defineProps({
  baseUnit: {
    type: Object,
    required: true
  },
  currentCfg: {
    type: Object,
    required: true
  },
  form: {
    type: Object,
    required: true
  },
  basicClass: {
    type: Number,
    required: true
  },
  handleOverQuantity: {
    type: Function,
    required: true
  }
})

const curRow = ref({})
const dialogVisible = ref(false)

const { maxHeight } = useMaxHeight(
  {
    mainBox: '.inbound-requisition-dialog',
    extraBox: ['.el-dialog__header'],
    wrapperBox: ['.el-dialog__body'],
    clientHRepMainH: true
  },
  computed(() => dialogVisible.value)
)

const branchQuantityArr = computed(() => {
  const arr = []
  let total = 0
  let spanMerge = 0
  curRow.value.applyPurchase.forEach((v, i) => {
    if (!v.isBranch) {
      if (spanMerge) {
        arr.push(...Array(spanMerge).fill(total))
      }
      total = v.quantity
      spanMerge = v.spanMerge
    } else {
      total += v.quantity
    }
    // 最后一行
    if (i === curRow.value.applyPurchase.length - 1) {
      arr.push(...Array(spanMerge).fill(total))
    }
  })
  return arr
})

function showDialog(row) {
  curRow.value = row
  curRow.value.applyPurchase.forEach((v) => {
    rowWatch(v)
  })
  dialogVisible.value = true
}

function rowWatch(row) {
  const isImmediate = Boolean(!row.theoryTotalWeight)
  if (props.basicClass & matClsEnum.SECTION_STEEL.V) {
    // 计算单件理论重量
    watch(
      [() => row.length, () => row.unitWeight, props.baseUnit],
      async () => {
        row.theoryWeight = await calcSectionSteelWeight({
          length: row.length, // 长度
          unitWeight: row.unitWeight // 单位重量
        })
      },
      { immediate: isImmediate }
    )
  }
  if (props.basicClass & matClsEnum.STEEL_PLATE.V) {
    // 计算单件理论重量
    watch(
      [() => row.length, () => row.width, () => row.thickness, props.baseUnit],
      async () => {
        row.theoryWeight = await calcSteelPlateWeight({
          name: row.classifyFullName, // 名称，用于判断是否为不锈钢，不锈钢与普通钢板密度不同
          length: row.length,
          width: row.width,
          thickness: row.thickness
        })
      },
      { immediate: isImmediate }
    )
  }
  // 计算总重
  watch(
    [() => row.theoryWeight, () => row.quantity],
    () => {
      calcTotalWeight(row)
    },
    { immediate: isImmediate }
  )
}

// 计算总重
function calcTotalWeight(row) {
  if (isNotBlank(row.theoryWeight) && row.quantity) {
    row.theoryTotalWeight = toPrecision(row.theoryWeight * row.quantity, props.baseUnit.weight.precision)
  } else {
    row.theoryTotalWeight = 0
  }
}

function addRow(index, list, row) {
  const sourceSpecInfo = props.form.selectObj?.[curRow.value.mergeId]
  const _row = reactive({
    ...row,
    sn: sourceSpecInfo?.sn,
    specificationLabels: sourceSpecInfo?.specificationLabels,
    serialNumber: sourceSpecInfo?.serialNumber,
    classifyId: sourceSpecInfo?.classifyId,
    classifyFullName: sourceSpecInfo?.classifyFullName,
    classifyName: sourceSpecInfo?.classifyName,
    classifyParentFullName: sourceSpecInfo?.classifyParentFullName,
    basicClass: sourceSpecInfo?.basicClass,
    specification: sourceSpecInfo?.specification,
    specificationMap: sourceSpecInfo?.specificationMap,
    measureUnit: sourceSpecInfo?.measureUnit,
    accountingUnit: sourceSpecInfo?.accountingUnit,
    measurePrecision: sourceSpecInfo?.measurePrecision,
    accountingPrecision: sourceSpecInfo?.accountingPrecision,
    unitWeight: sourceSpecInfo?.unitWeight,
    length: sourceSpecInfo?.length,
    width: sourceSpecInfo?.width,
    thickness: sourceSpecInfo?.thickness,
    theoryTotalWeight: null,
    quantity: null,
    originQuantity: null,
    isBranch: true
  })
  rowWatch(_row)
  list.splice(index + 1, 0, _row)
  const originIndex = findOriginIndex(index, list, row)
  if (!list[originIndex].spanMerge) {
    list[originIndex].spanMerge = 1
  }
  list[originIndex].spanMerge++
}

// 删除行
function delRow(index, list, row) {
  if (row.isBranch) {
    const originIndex = findOriginIndex(index, list, row)
    list[originIndex].spanMerge--
  } else {
    list[index + 1].spanMerge = row.spanMerge - 1
    list[index + 1].isBranch = false
  }
  list.splice(index, 1)
}

// 找到原始数据的索引
function findOriginIndex(index, list, row) {
  let originIndex = index
  for (let i = index; i >= 0; i--) {
    if (!list[i].isBranch) {
      originIndex = i
      break
    }
  }
  return originIndex
}

function spanMethod({ row, column, rowIndex, columnIndex }) {
  if (['serialNumber', 'project', 'purchaseQuantity', 'inboundQuantity'].includes(column.property)) {
    if (row.isBranch) {
      return {
        rowspan: 1,
        colspan: 0
      }
    } else {
      return {
        rowspan: row.spanMerge || 1,
        colspan: 1
      }
    }
  }
}
</script>

<style lang="scss" scoped></style>
