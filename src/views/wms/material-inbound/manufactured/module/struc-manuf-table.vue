<template>
  <common-table
    ref="tableRef"
    v-bind="$attrs"
    :data="form.strucManufList"
    :cell-class-name="wrongCellMask"
    :expand-row-keys="expandRowKeys"
    :show-empty-symbol="false"
    return-source-data
    row-key="uid"
    @select="selectTableChange"
    @select-all="selectAllTableChange"
  >
    <el-table-column v-if="!props.boolPartyA" type="selection" width="55" align="center" :selectable="selectable" />
    <el-expand-table-column :data="form.strucManufList" v-model:expand-row-keys="expandRowKeys" row-key="uid" fixed="left">
      <template #default="{ row }">
        <div class="mtb-10">
          <el-input
            v-model="row.remark"
            :rows="1"
            :autosize="{ minRows: 1, maxRows: 1 }"
            type="textarea"
            placeholder="备注"
            maxlength="200"
            show-word-limit
            style="width: 400px"
          />
        </div>
      </template>
    </el-expand-table-column>
    <el-table-column label="序号" type="index" align="center" width="60" fixed="left" />
    <el-table-column prop="monomer.name" label="单体" align="center" show-overflow-tooltip min-width="120px" />
    <el-table-column prop="area.name" label="区域" align="center" show-overflow-tooltip min-width="120px" />
    <el-table-column prop="name" label="名称" align="center" show-overflow-tooltip min-width="100px" />
    <el-table-column prop="serialNumber" label="编号" align="center" show-overflow-tooltip min-width="100px" />
    <el-table-column prop="specification" label="规格" align="center" show-overflow-tooltip min-width="140px" />
    <el-table-column prop="length" label="长度（mm）" align="center" show-overflow-tooltip />
    <el-table-column prop="material" label="材质" align="center" show-overflow-tooltip />
    <el-table-column prop="purchaseQuantity" label="采购数量" align="center" show-overflow-tooltip />
    <el-table-column prop="quantity" label="本次实收数" align="center" min-width="120px">
      <template #default="{ row }">
        <common-input-number
          v-model="row.quantity"
          :min="0"
          :max="row.canPurchaseQuantity"
          :controls="false"
          :step="1"
          size="mini"
          placeholder="本次实收数"
        />
      </template>
    </el-table-column>
    <el-table-column prop="mete" label="实收量(kg)" align="center" min-width="120px" />
  </common-table>
</template>

<script setup>
import { defineExpose, defineProps, watchEffect, computed, ref, watch } from 'vue'
import { positiveNumPattern } from '@/utils/validate/pattern'
import { isNotBlank, toPrecision } from '@/utils/data-type'

import { regExtra } from '@/composables/form/use-form'
import useTableValidate from '@compos/form/use-table-validate'
import elExpandTableColumn from '@comp-common/el-expand-table-column.vue'

const props = defineProps({
  boolPartyA: {
    type: Boolean,
    default: false
  },
  fillableAmount: {
    type: Boolean,
    default: false
  }
})

const tableRef = ref()
const { form } = regExtra() // 表单
const expandRowKeys = ref([]) // 展开行key

const rules = {
  quantity: [
    { required: true, message: '请填写数量', trigger: 'blur' },
    { pattern: positiveNumPattern, message: '数量必须大于0', trigger: 'blur' }
  ]
}

const tableRules = computed(() => {
  const _rules = Object.assign({}, rules)
  return _rules
})

const { tableValidate, wrongCellMask } = useTableValidate({ rules: tableRules }) // 表格校验

function selectable(row, rowIndex) {
  return !!row.canPurchaseQuantity || true
}

function selectTableChange(select, row) {
  const boolSelect = Boolean(select.findIndex((v) => v.id === row.id) !== -1)
  form.selectObj[row.purchaseOrderDetailId].isSelected = boolSelect
}

function selectAllTableChange(select) {
  const boolSelect = Boolean(select?.length)
  form.strucManufList.forEach((v) => {
    form.selectObj[v.purchaseOrderDetailId].isSelected = boolSelect
  })
}

function rowWatch(row) {
  watchEffect(() => {
    if (!props.boolPartyA && isNotBlank(form.selectObj?.[row.purchaseOrderDetailId])) {
      const _isSelected = form.selectObj[row.purchaseOrderDetailId]?.isSelected
      form.selectObj[row.purchaseOrderDetailId] = {
        ...form.selectObj[row.purchaseOrderDetailId],
        ...row,
        isSelected: _isSelected
      }
    }
  })
  // 计算总重
  watch([() => row.quantity, () => row.netWeight], () => calcTotalWeight(row), { immediate: true })
}

// 计算总重
function calcTotalWeight(row) {
  if (isNotBlank(row.netWeight) && row.quantity) {
    row.mete = toPrecision(row.netWeight * row.quantity)
  } else {
    row.mete = 0
  }
}

// 校验
function validate() {
  const _list = form.strucManufList.filter((v) => {
    if (props.boolPartyA || form.selectObj[v.purchaseOrderDetailId]?.isSelected) {
      return true
    } else {
      return false
    }
  })
  const { validResult } = tableValidate(_list)
  // form.strucManufList = dealList
  return validResult
}

function toggleRowSelection(row, selected) {
  tableRef?.value?.toggleRowSelection(row, selected)
}

defineExpose({
  validate,
  toggleRowSelection,
  rowWatch
})
</script>
