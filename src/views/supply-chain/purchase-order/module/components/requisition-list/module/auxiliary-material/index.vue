<template>
  <common-table v-bind="$attrs" :data="list" :cell-class-name="wrongCellMask" :show-empty-symbol="false" return-source-data row-key="uid">
    <el-table-column label="序号" type="index" align="center" width="60" fixed="left" />
    <el-table-column label="申购单号" prop="purchaseSN" fixed="left" width="140" align="center">
      <template #default="{ row }">
        <table-cell-tag
          :show="row.boolPurchase"
          name="已采购"
          color="#e6a23c"
        />
        <span>{{ row.purchaseSN }}</span>
      </template>
    </el-table-column>
    <el-table-column prop="serialNumber" show-overflow-tooltip label="编号" align="center" fixed="left" />
    <el-table-column prop="classifyName" label="物料种类" align="center" fixed="left" show-overflow-tooltip>
      <template #default="{ row }">
        <el-tooltip :content="row.classifyParentFullName" :disabled="!row.classifyParentFullName" :show-after="500" placement="top">
          <span v-empty-text="row.classifyName" />
        </el-tooltip>
      </template>
    </el-table-column>
    <el-table-column prop="specification" label="规格" align="center" fixed="left" show-overflow-tooltip>
      <template #default="{ row }">
        <el-tooltip :content="row.specificationLabels" placement="top">
          <span v-empty-text="row.specification" />
        </el-tooltip>
      </template>
    </el-table-column>
    <el-table-column prop="measureUnit" label="计量单位" align="center">
      <template #default="{ row }">
        <span v-empty-text>{{ row.measureUnit }}</span>
      </template>
    </el-table-column>
    <el-table-column prop="quantity" label="数量" align="center">
      <template #default="{ row }">
        <common-input-number
          v-if="row.measureUnit && Boolean(currentCfg?.quantity & basicClass)"
          v-model="row.quantity"
          :min="0"
          :max="999999999"
          :controls="false"
          :step="1"
          :precision="row.measurePrecision"
          size="mini"
          placeholder="数量"
        />
        <span v-else v-empty-text>{{ row.quantity }}</span>
      </template>
    </el-table-column>
    <el-table-column prop="accountingUnit" label="核算单位" align="center">
      <template #default="{ row }">
        <span v-empty-text>{{ row.accountingUnit }}</span>
      </template>
    </el-table-column>
    <el-table-column prop="mete" label="核算量" align="center">
      <template #default="{ row }">
        <common-input-number
          v-if="Boolean(currentCfg?.mete & basicClass)"
          v-model="row.mete"
          :min="0.000001"
          :max="999999999"
          :controls="false"
          :step="1"
          :precision="row.accountingPrecision"
          size="mini"
          placeholder="核算量"
        />
        <span v-else>{{ row.mete }}</span>
      </template>
    </el-table-column>

    <el-table-column prop="color" label="颜色" align="center">
      <template #default="{ row }">
        <el-input v-model.trim="row.color" maxlength="20" size="mini" placeholder="颜色" />
      </template>
    </el-table-column>
    <el-table-column prop="brand" label="品牌" align="center">
      <template #default="{ row }">
        <el-input v-model.trim="row.brand" maxlength="60" size="mini" placeholder="品牌" />
      </template>
    </el-table-column>
    <el-table-column label="操作" width="90" align="center" fixed="right">
      <template #default="{ row, $index }">
        <common-button icon="el-icon-plus" :disabled="isExist(row.id) || row.boolPurchase" type="warning" size="mini" @click="addRow(row, $index)" />
      </template>
    </el-table-column>
  </common-table>
</template>

<script setup>
import { defineExpose, defineEmits, inject, ref } from 'vue'
import { matClsEnum } from '@/utils/enum/modules/classification'
import { positiveNumPattern } from '@/utils/validate/pattern'

import useWmsConfig from '@/composables/store/use-wms-config'
import useTableValidate from '@compos/form/use-table-validate'
import { deepClone } from '@/utils/data-type'

// 当前物料基础类型
const basicClass = matClsEnum.MATERIAL.V

const { purchaseCfg: currentCfg } = useWmsConfig()

const emit = defineEmits(['add-purchase'])
const form = inject('crud')?.form
const list = ref([])

// 数量校验方式
const validateQuantity = (value, row) => {
  if (row.measureUnit) return !!value

  return true
}

const rules = {
  quantity: [{ validator: validateQuantity, message: '请填写数量', trigger: 'blur' }],
  mete: [
    { required: true, message: '请填写核算量', trigger: 'blur' },
    { pattern: positiveNumPattern, message: '核算量必须大于0', trigger: 'blur' }
  ]
}

const { tableValidate, wrongCellMask } = useTableValidate({ rules: rules }) // 表格校验

function isExist(id) {
  return form.list?.findIndex((v) => v.id === id) !== -1
}

function addRow(row, index) {
  emit('add-purchase', row, index)
}

// 校验
function validate(list) {
  const { validResult } = tableValidate(list)
  return validResult
}

function initList(_list) {
  list.value = deepClone(_list)
}

defineExpose({
  initList,
  validate
})
</script>
