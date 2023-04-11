<template>
  <el-table-column prop="quantity" align="center" width="110px" :label="`本次实收数 (${baseUnit.measure.unit})`">
    <template #default="{ row }">
      <common-input-number
        v-if="!row.boolApplyPurchase && Boolean(currentCfg?.quantity & basicClass) && form.selectObj?.[row.mergeId]?.isSelected"
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
      <template v-else>
        <span>{{ row.quantity }}</span>
        <el-popover placement="top" :width="700" trigger="click">
          <template #reference>
            <span style="margin-left: 5px; cursor: pointer">
              <el-edit
                v-if="Boolean(currentCfg?.quantity & basicClass) && form.selectObj?.[row.mergeId]?.isSelected"
                class="el-icon"
                style="color: #1881ef; vertical-align: middle"
              />
              <el-icon-view v-else class="el-icon" style="color: #1881ef; vertical-align: middle" />
            </span>
          </template>
          <common-table :data="row.applyPurchase" style="width: 100%" return-source-data :show-empty-symbol="false">
            <el-table-column label="序号" type="index" align="center" width="60" />
            <el-table-column prop="serialNumber" :show-overflow-tooltip="true" label="申购单号" min-width="140" align="center" />
            <el-table-column prop="project" :show-overflow-tooltip="true" label="项目" min-width="140" align="center">
              <template #default="{ row }">
                <span v-if="row.project">{{ projectNameFormatter(row.project) }}</span>
                <span v-else>-</span>
              </template>
            </el-table-column>
            <el-table-column
              prop="purchaseQuantity"
              :show-overflow-tooltip="true"
              :label="`采购数量 (${baseUnit.measure.unit})`"
              min-width="100"
              align="center"
            />
            <el-table-column
              v-if="Boolean(currentCfg?.quantity & basicClass) && form.selectObj?.[row.mergeId]?.isSelected"
              prop="quantity"
              :show-overflow-tooltip="true"
              :label="`本次实收数 (${baseUnit.measure.unit})`"
              min-width="135px"
              align="center"
            >
              <template #default="{ row: purRow }">
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
                  @blur="handleOverQuantity(purRow, true)"
                />
              </template>
            </el-table-column>
          </common-table>
        </el-popover>
      </template>
    </template>
  </el-table-column>
</template>

<script setup>
import { defineProps } from 'vue'
import { projectNameFormatter } from '@/utils/project'

defineProps({
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
</script>

<style lang="scss" scoped></style>
